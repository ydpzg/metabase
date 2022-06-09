import React from "react";
import { ComponentStory } from "@storybook/react";
import { useArgs } from "@storybook/client-api";
import NumberWidget from "./NumberWidget";

export default {
  title: "Parameters/NumberWidget",
  component: NumberWidget,
};

const Template: ComponentStory<typeof NumberWidget> = args => {
  const [{ value }, updateArgs] = useArgs();

  const handleSetValue = (v: string) => {
    updateArgs({ value: v });
  };

  return <NumberWidget {...args} value={value} setValue={handleSetValue} />;
};

export const Default = Template.bind({});
Default.args = {
  value: [1],
};

export const TwoArgs = Template.bind({});
TwoArgs.args = {
  value: [1, 2],
  arity: 2,
  infixText: "and",
};

export const ThreeArgs = Template.bind({});
ThreeArgs.args = {
  value: [1, 2],
  arity: 3,
  infixText: "foo",
  autoFocus: true,
};

export const NArgs = Template.bind({});
NArgs.args = {
  value: [1, 2, 3, 4, 5, 6],
  arity: "n",
  autoFocus: true,
};