import { WebSocketContext } from "./WebSocketProvider";
import { useContext, useEffect, useState } from "react";
import CheckCircleIcon from "@mui/icons-material/CheckCircle";
import AutorenewIcon from "@mui/icons-material/Autorenew";
import ErrorIcon from "@mui/icons-material/Error";
import { ReadyState } from "react-use-websocket";
import { IconButton, Tooltip, Typography } from "@mui/material";

const Connecting = () => (
  <AutorenewIcon
    sx={{
      animation: "spin 2s linear infinite",
      "@keyframes spin": {
        "0%": { transform: "rotate(0deg)" },
        "100%": { transform: "rotate(360deg)" },
      },
    }}
  />
);

const Connected = () => <CheckCircleIcon color="success" />;

const NotConnected = () => <ErrorIcon color="error" />;

export default function ConnectionStatus() {
  const ws = useContext(WebSocketContext);
  const [readyState, setReadyState] = useState<ReadyState>(
    ReadyState.UNINSTANTIATED,
  );
  const label =
    readyState === ReadyState.CONNECTING
      ? "Connecting"
      : readyState === ReadyState.OPEN
        ? "Connected"
        : "Not Connected";
  useEffect(() => {
    setReadyState(ws.readyState);
  }, [ws.readyState]);
  return (
    <>
      <Tooltip title={label}>
        <IconButton
          disabled={
            readyState === ReadyState.CONNECTING ||
            readyState === ReadyState.OPEN
          }
        >
          {readyState === ReadyState.CONNECTING ? (
            <Connecting />
          ) : readyState === ReadyState.OPEN ? (
            <Connected />
          ) : (
            <NotConnected />
          )}
        </IconButton>
      </Tooltip>
      <Typography sx={{ display: { xs: "none", sm: "inline" } }}>
        {label}
      </Typography>
    </>
  );
}
