import { formatValue } from "metabase/lib/formatting";

import { getParameterType, isFieldFilterParameter } from "./parameter-type";
import { formatDateValue } from "./date-formatting";
import { UiParameter } from "../types";

function inferValueType(parameter: UiParameter) {
  const type = getParameterType(parameter);
  if (type === "number") {
    return "type/Number";
  }

  return "type/Text";
}

function formatWithInferredType(value: any, parameter: UiParameter) {
  const inferredType = inferValueType(parameter);
  const column = {
    base_type: inferredType,
  };
  return formatValue(value, {
    column,
    maximumFractionDigits: 20,
  });
}

export function formatParameterValue(value: any, parameter: UiParameter) {
  const type = getParameterType(parameter);
  if (type === "date") {
    return formatDateValue(value, parameter);
  }

  if (isFieldFilterParameter(parameter)) {
    // skip formatting field filter parameters mapped to native query variables
    if (parameter.hasOnlyFieldTargets === false) {
      return value;
    }

    // format using the parameter's first targeted field
    if (parameter.fields.length > 0) {
      const [firstField] = parameter.fields;
      // when a parameter targets multiple fields we won't know
      // which parameter the value is associated with, meaning we
      // are unable to remap the value to the correct field
      const remap = parameter.fields.length === 1;
      return formatValue(value, {
        column: firstField,
        maximumFractionDigits: 20,
        remap,
      });
    }
  }

  // infer type information from parameter type
  return formatWithInferredType(value, parameter);
}
