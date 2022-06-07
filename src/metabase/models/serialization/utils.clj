(ns metabase.models.serialization.utils
  "Defines several helper functions and protocols for the serialization system.
  Serialization is an enterprise feature, but in the interest of keeping all the code for an entity in one place, these
  methods are defined here and implemented for all the exported models.

  Whether to export a new model:
  - Generally, the high-profile user facing things (databases, questions, dashboards, snippets, etc.) are exported.
  - Internal or automatic things (users, activity logs, permissions) are not.

  If the model is not exported, add it to the exclusion lists in the tests.

  For models that are exported, you have to implement this file's protocols and multimethods for it:
  - All exported models should either have an CHAR(21) column `entity_id`, or a portable external name (like a database
    URL).
  - identity-hash-fields should give the list of fields that distinguish an instance of this model from another, on a
    best-effort basis.
    - Use things like names, labels, or other stable identifying features.
    - NO numeric database IDs!
    - Any foreign keys should be hydrated and the identity-hash of the foreign entity used as part of the hash.
      - There's a `hydrated-hash` helper for this with several example uses."
  (:require [potemkin.types :as p.types]
            [toucan.db :as db]
            [toucan.hydrate :refer [hydrate]]
            [toucan.models :as models]))

;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                              Identity Hashes                                                   |
;;; +----------------------------------------------------------------------------------------------------------------+
;;; Generated entity_id values have lately been added to most exported models, but they only get populated on newly
;;; created entities. Since we can't rely on entity_id being present, we need a content-based definition of identity for
;;; all exported models.
(p.types/defprotocol+ IdentityHashable
  (identity-hash-fields
    [entity]
    "You probably want to call metabase.models.serialization/identity-hash instead of calling this directly.

    Content-based identity for the entities we export in serialization. This should return a sequence of functions to
    apply to an entity and then hash. For example, Metric's identity-hash-fields is [:name :table]. These functions are
    mapped over each entity and clojure.core/hash called on the result. This gives a portable hash value across JVMs,
    Clojure versions, and platforms.

    NOTE: No numeric database IDs! For any foreign key, use `hydrated-hash` to hydrate the foreign entity and include
    its identity-hash as part of this hash. This is a portable way of capturing the foreign relationship."))

(defn raw-hash
  "Hashes a Clojure value into an 8-character hex string, which is used as the identity hash.
  Don't call this outside a test, use identity-hash instead."
  [target]
  (format "%08x" (hash target)))

(defn identity-hash
  "Given a modeled entity, return its identity hash for use in serialization. The hash is an 8-character hex string.
  The hash is based on a set of fields on the entity, defined by its implementation of `identity-hash-fields`.
  These hashes are intended to be a decently robust fallback for older entities whose `entity_id` fields are not
  populated."
  [entity]
  (-> (for [f (identity-hash-fields entity)]
        (f entity))
      raw-hash))

(defn hydrated-hash
  "Many entities reference other entities. Using the autoincrementing ID is not portable, so we use the identity-hash
  of the referenced entity. This is a helper for writing `identity-hash-fields`."
  [hydration-key]
  (fn [entity]
    (-> entity
        (hydrate hydration-key)
        (get hydration-key)
        identity-hash)))

;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                          Serialization Process                                                 |
;;; +----------------------------------------------------------------------------------------------------------------+

;;; Utilities for writing serialization functions.
(defn remove-timestamps
  "Removes any key that ends with _at."
  [m]
  (let [ats (filter #(.endsWith (str %) "_at") (keys m))]
    (apply dissoc m ats)))

;;; The end result of serialization is a directory tree of YAML files. Some entities are written to individual files
;;; (most of them, eg. collections and fields) while others are consolidated into a single file for the entire set (eg.
;;; settings).
;;;
;;; Some entities cannot be loaded until others are also loaded, these are its *dependencies*. For example, a Field
;;; can't be loaded until its Table is defined, and the Table can't be loaded without the Database. These dependencies
;;; are derived from the serialized form, and then loaded recursively in postorder.
(defmulti serdes-dependencies
  "Given an entity map AS DECODED FROM YAML, returns a (possibly empty) list of its dependencies, where each dependency
  is represented by `(or entity_id identity-hash)` of the target entity.

  This is mostly part of the deserialization process, so it's based on the serialized map and not on an IModel instance.

  Default implementation returns an empty vector, so only models that have dependencies need to implement this."
  ; TODO What actually is the right key for the entity type?
  :name)

(defmethod serdes-dependencies :default [_]
  [])

(p.types/defprotocol+ ISerializable
  ;;; Serialization side
  (serdes-combined-file?
    [this]
    "Given a model, returns true if it should be dumped as a single file (eg. Settings) or false to dump as one file per
    entity (the majority).
    Default implementation returns false, to dump as one file per entity.")

  (serdes-serialize-all
    [this]
    "ONLY CALLED FOR COMBINED FILE OUTPUT.
    Default implementation throws an error; this needs to be custom implemented.")

  (serdes-query
    [this user-or-nil]
    "Performs the select query, possibly filtered, for all the entities of this type that should be serialized.
    Returns the result of the db/select.
    Defaults to a naive db/select for the entire model, ignoring the optional user argument.
    ONLY CALLED FOR MULTI-FILE OUTPUT.")

  (serdes-serialize-one
    [this]
    "Serializes a single entity into a YAML-friendly map, with its filename.
    Actually returns {file-name {entity map...}}.

    Default implementation:
    - Uses $ENTITY_ID.yaml or $IDENTITY_HASH.yaml as the filename.
    - Returns the entity as a vanilla map with :id and any :foo_at fields removed.
    - Adds the field :type \"Collection\" or similar.

    That suffices for a few simple entities, but most entities will need to override this.")

  ;;; Deserialization side.
  (serdes-upsert
    [old-entity new-map]
    "Given the original entity and the new deserialized map (not an IModel yet), upsert appdb to merge in the updated
    entity.
    Defaults to a naive update by primary key, using just the new map's values.")

  (serdes-insert
    [this new-map]
    "Given the model (eg. Collection) and the new deserialized map (not an IModel), insert this new record into
    the appdb.
    Defaults to a straight db/insert! of this new map."))

(defn- default-query [model _]
  (db/select model))

(defn- default-serialize-one [entity]
  (let [pk (models/primary-key entity)]
    (-> (into {} entity)
        (dissoc pk)
        (remove-timestamps))))

(defn- default-upsert [old-entity new-map]
  (db/update! old-entity (get old-entity (models/primary-key old-entity)) new-map))

(defn- default-insert [model new-map]
  (db/insert! model new-map))

(def ISerializableDefaults
  {:serdes-combined-file? (constantly false)
   :serdes-serialize-all  (fn [this]
                            (throw (ex-info (format "Unimplemented serdes-serialize-all for %s" (name this)) {})))
   :serdes-query          default-query
   :serdes-serialize-one  default-serialize-one
   :serdes-upsert         default-upsert
   :serdes-insert         default-insert})

;;; Serialization stack:
;;; (dump target-path user-or-nil)
;;; - This gets a list of types, eg. "Database", "Field", "Collection", etc.
;;; - It calls serdes-combined-file?, which defaults to false but a few types get combined (eg. Settings)
;;;   - Multi-file: It calls (serdes-query EntityType user-or-nil) to get the result set, then calls
;;;     (serdes-serialize-one entity) with each one.
;;;   - Single-file: It calls (serdes-serialize-all EntityType).
;;; - In either case, the return value is a map of {file-path clojure-map}.
;;;   - The above are expected to do some filtering of fields that shouldn't be exported. By default that's anything
;;;     that ends with _at.
;;;   - References to other entities eg. collection_id should be replaced with entity_id or identity-hash of the target,
;;;     rather than its database key.
;;; - The dump system takes care of actually writing the files and directories, and the conversion to YAML.
;;;   - Returning as Clojure data makes testing easier.

;;; Deserialization stack:
;;; (load root-path)
;;; - This recursively reads in the entire directory tree, converting from YAML into Clojure structures, and building a
;;;   map of entity_id -> map and identity_hash -> map for all the entities that exist.
;;; - Scan all the serialized types (which we have a list of) and build two maps of {"EntityType" {entity_id entity}}
;;;   and {"EntityType" {identity-hash entity}}.
;;;   - This isn't great for performance but identity hash values can't be used in a WHERE clause...
;;; - Load all entities in an arbitrary order with (serdes-deserialize-one ctx map) (NOT a multimethod) which:
;;;   - Checks if this has already been loaded: (get-in ctx [:loaded (or entity_id identity-hash)])
;;;     - Just return the cached value if it's already been done.
;;;   - Recursively does (serdes-deserialize-one ctx dep) on all the deps.
;;;   - Tries to look up the corresponding entry entity_id and identity-hash in the maps above.
;;;     - If a match is found, do an upsert with (serdes-upsert old new).
;;;     - If no match is found, do a (serdes-insert new).
;;;     - These default to straight Toucan function pass-through but can be overridden if extra work is needed.
