export default function resolveWebSocketUrl(): string {
  if (typeof IS_DEVELOPMENT === "boolean" && IS_DEVELOPMENT) {
    return "ws://localhost:30179";
  } else {
    return `ws://${window.location.host}`;
  }
}
