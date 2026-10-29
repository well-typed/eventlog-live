import { useContext } from "react";
import { CursorSyncContext } from "./CursorSyncProvider";
import Switch, { SwitchProps } from "@mui/material/Switch";
import Typography from "@mui/material/Typography";
import Tooltip from "@mui/material/Tooltip";

export type CursorSyncSwitchProps = Omit<SwitchProps, "checked" | "onChange">;

export default function CursorSyncSwitch(props: CursorSyncSwitchProps) {
  const cursorSyncContext = useContext(CursorSyncContext);
  return (
    <>
      <Tooltip
        sx={{ display: { xs: "none", sm: "inherit" } }}
        title="Synchonize the cursor position and view between all plots."
      >
        <Switch
          checked={cursorSyncContext.cursorSync}
          onChange={cursorSyncContext.toggleCursorSync}
          {...props}
        />
      </Tooltip>
      <Typography sx={{ display: { xs: "none", md: "inherit" } }}>
        Cursor&nbsp;
      </Typography>
      <Typography
        sx={{ display: { xs: "none", sm: "inherit" }, marginRight: 1 }}
      >
        Sync
      </Typography>
    </>
  );
}
