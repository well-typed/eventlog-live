import Button, { ButtonProps } from "@mui/material/Button";
import Tooltip from "@mui/material/Tooltip";
import { useContext, useState } from "react";
import { ServiceContext } from "./ServiceTabs";
import resolveControlBaseUrl from "@/util/resolveControlUrl";
import Snackbar, { SnackbarCloseReason } from "@mui/material/Snackbar";
import Alert, { AlertProps } from "@mui/material/Alert";

export type ControlButtonProps = ButtonProps & {
  label: string;
  title?: string;
  feedback?: {
    onSend?: string;
    onReceive?: string;
    onSuccess?: string;
  };
  namespace: string;
  commandId: string | number;
};

interface ControlFeedback {
  severity: AlertProps["severity"];
  message: string;
}

export default function ControlButton(props: ControlButtonProps) {
  const { label, title, namespace, commandId, ...buttonProps } = props;
  const { onSend = "Request sent.", onReceive = "Request received." } =
    props.feedback ?? {};

  // The context that provides the service information.
  const service = useContext(ServiceContext);

  // The snackbar state for control feedback.
  const [feedback, setFeedback] = useState<ControlFeedback | null>(null);

  // The button onClick handler.
  const handleClick = async (_event: React.MouseEvent<HTMLButtonElement>) => {
    // Provide feedback.
    setFeedback({ severity: "info", message: onSend });

    // Send the request.
    const url = new URL(
      `/control/${namespace}/${commandId}`,
      resolveControlBaseUrl(),
    );
    // TODO: If same-origin, don't use no-cors and handle responses.
    const response = await fetch(url, {
      method: "POST",
      headers: new Headers({
        "content-type": "application/x-www-form-urlencoded",
      }),
      mode: "no-cors",
      body: `service-name=${service.name}`,
    });
    if (response.type === "opaque") {
      setFeedback({ severity: "info", message: onReceive });
      return;
    }
  };

  // The snackbar onClose handler.
  const handleClose = async (
    _event: React.SyntheticEvent,
    _reason?: SnackbarCloseReason,
  ) => {
    setFeedback(null);
  };

  return (
    <>
      <Tooltip title={title}>
        <Button
          onClick={handleClick}
          disabled={feedback !== null}
          {...buttonProps}
        >
          {label}
        </Button>
      </Tooltip>
      <Snackbar
        open={feedback !== null}
        autoHideDuration={5_000}
        onClose={handleClose}
      >
        <Alert
          severity={feedback?.severity ?? "info"}
          variant="filled"
          onClose={handleClose}
          sx={{ width: "100%" }}
        >
          {feedback?.message}
        </Alert>
      </Snackbar>
    </>
  );
}
