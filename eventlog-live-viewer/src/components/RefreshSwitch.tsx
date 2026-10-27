import { useContext } from "react";
import { RefreshContext } from "./RefreshProvider";
import Switch, { SwitchProps } from "@mui/material/Switch";
import Typography from "@mui/material/Typography";
import Tooltip from "@mui/material/Tooltip";

type RefreshSwitchProps = Omit<SwitchProps, "checked" | "onChange">;

function RefreshSwitch(props: RefreshSwitchProps) {
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
        sx={{ display: { xs: "none", sm: "inline" }, marginRight: 1 }}
      >
        Refresh
      </Typography>
    </>
  );
}

export default RefreshSwitch;
