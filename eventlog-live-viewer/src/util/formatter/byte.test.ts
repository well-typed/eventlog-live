import { describe, expect, test } from "@jest/globals";
import format from "./byte";

describe("byte formatter", () => {
  test.each([
    { bytes: 1_200, expected: "1.2 KB" },
    { bytes: 1_200_000, expected: "1.2 MB" },
    { bytes: 1_200_000_000, expected: "1.2 GB" },
  ])("format bytes", ({ bytes, expected }) => {
    expect(format(bytes, 1)).toBe(expected);
  });
});
