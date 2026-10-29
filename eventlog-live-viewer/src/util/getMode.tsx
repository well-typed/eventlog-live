/**
 * Get the effective mode from the mode and systemMode.
 */
export function getMode(
  mode?: "light" | "dark" | "system",
  systemMode?: "light" | "dark",
): "light" | "dark" | undefined {
  return mode === "system" ? systemMode : mode;
}
