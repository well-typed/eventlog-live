"use client";

import Plot from "@/components/plots/Plot";
import * as z from "@/util/z";
import { useTheme } from "@mui/material";
import useMediaQuery from "@mui/material/useMediaQuery";
import { useEffect, useState } from "react";

const heapProfSampleByClosureTypeAttrs = z.strictObject({
  name: z.literal("heapProfSample"),
  heapProfBreakdown: z.literal("-hT"),
  heapProfId: z.literal(0),
  heapProfLabel: z.string(),
  heapProfSampleEra: z.number(),
});

const heapProfSampleAttrs = z.union([heapProfSampleByClosureTypeAttrs]);

const getSize = (lg: boolean, xl: boolean): [number, number] =>
  xl ? [1200, 800] : lg ? [900, 600] : [600, 400];

export const HeapProfSamplePlot = () => {
  const theme = useTheme();
  const lg = useMediaQuery(theme.breakpoints.up("lg"));
  const xl = useMediaQuery(theme.breakpoints.up("xl"));
  const [size, setSize] = useState<[number, number]>(getSize(lg, xl));

  useEffect(() => {
    setSize(getSize(lg, xl));
  }, [lg, xl]);

  return (
    <Plot
      name="heapProfSample"
      validateAttrs={(attrs) => z.validate(heapProfSampleAttrs, attrs)}
      getSeriesLabel={(attrs) => attrs.heapProfLabel}
      getSeriesOptions={(label) => ({
        label,
        spanGaps: true,
      })}
      unit="By"
      seriesLimit={10}
      totalSeries={{ label: "Total" }}
      otherSeries={{ label: "Other" }}
      options={{
        title: "Heap Profile",
        width: size[0],
        height: size[1],
        scales: {
          x: { time: true },
          y: { time: false },
        },
      }}
    />
  );
};
