import { describe, expect, test } from "@jest/globals";
import sum from "./sum";
import zipWith from "./zipWith";

describe("zipWith", () => {
  test("zipWith(sum, [[1], [2], [3]]) === [6]", () => {
    expect(zipWith(sum, [[1], [2], [3]])).toStrictEqual([6]);
  });
});
