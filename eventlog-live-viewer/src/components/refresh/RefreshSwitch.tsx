import { useContext } from "react";
import { RefreshContext } from "./RefreshProvider";
import Switch, { SwitchProps } from "@mui/material/Switch";
import Typography from "@mui/material/Typography";
import Tooltip from "@mui/material/Tooltip";

export type RefreshSwitchProps = Omit<SwitchProps, "checked" | "onChange">;

export default function RefreshSwitch(props: RefreshSwitchProps) {
  const refreshContext = useContext(RefreshContext);
  return (
    <>
      <Tooltip title="Refresh">
        <Switch
          checked={refreshContext.refresh}
          onChange={refreshContext.toggleRefresh}
          {...props}
        />
      </Tooltip>
      <Typography
        sx={{ display: { xs: "none", sm: "inherit" }, marginRight: 1 }}
      >
        Refresh
      </Typography>
    </>
  );
}
