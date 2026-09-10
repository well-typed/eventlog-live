"use client";

import { useContext } from "react";
import { RefreshContext } from "./RefreshProvider";
import Switch, { SwitchProps } from "@mui/material/Switch";

type RefreshInputProps = Omit<SwitchProps, "checked" | "onChange">;

function RefreshInput(props: RefreshInputProps) {
  const refreshContext = useContext(RefreshContext);
  return (
    <Switch
      checked={refreshContext.refresh}
      onChange={refreshContext.toggleRefresh}
      {...props}
    />
  );
}

export default RefreshInput;
