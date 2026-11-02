import { describe, expect, test } from "@jest/globals";
import sum from "./sum";

describe("sum", () => {
  test("sum([1, 2, 3]) === 6", () => {
    expect(sum([1, 2, 3])).toBe(6);
  });
});
