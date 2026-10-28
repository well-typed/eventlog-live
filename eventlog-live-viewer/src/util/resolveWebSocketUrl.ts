export default function resolveWebSocketUrl(): string {
  return `ws://${URL.parse(window.location.href)?.host ?? "localhost:30179"}`;
}
