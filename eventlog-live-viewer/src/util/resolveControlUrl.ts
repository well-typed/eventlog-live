export default function resolveControlBaseUrl(): string {
  if (typeof IS_DEVELOPMENT === "boolean" && IS_DEVELOPMENT) {
    return "http://localhost:30179";
  } else {
    return `http://${window.location.host}`;
  }
}
