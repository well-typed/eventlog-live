export default function resolveWebSocketUrl(): string {
  if (typeof EVENTLOG_LIVE_VIEWER_WEBSOCKET_URL === "string") {
    return EVENTLOG_LIVE_VIEWER_WEBSOCKET_URL;
  } else {
    return `ws://${URL.parse(window.location.href)?.host ?? "localhost:30179"}`;
  }
}
