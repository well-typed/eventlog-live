"use client";

import WebSocketProvider from "@/components/WebSocketProvider";
import ExportRequestProvider from "@/components/ExportRequestProvider";
import Plot, { PlotOptions, SeriesIndex, SeriesLabel } from "@/components/Plot";
import RefreshProvider from "@/components/RefreshProvider";
import RefreshInput from "@/components/RefreshInput";
import Grid from "@mui/material/Grid";
import Container from "@mui/material/Container";
import ConnectionStatus from "@/components/ConnectionStatus";
import AppBar from "@mui/material/AppBar";
import Toolbar from "@mui/material/Toolbar";
import Box from "@mui/material/Box";
import { styled } from "@mui/material";
import * as z from "../util/z";

/* Color Cycle */

const colorCycle = [
  "blue",
  "red",
  "green",
  "purple",
  "maroon",
  "olive",
  "navy",
  "fuchsia",
  "lime",
  "aqua",
  "teal",
];

/* HeapSize Attributes */

const heapSizeAttrs = z.catchall(
  z.strictObject({ name: z.string(), capNo: z.number() }),
  z.any(),
);

/* GC Stats Attributes */

const gcStatsAttrs = z.catchall(
  z.strictObject({ name: z.string(), capNo: z.number(), gen: z.number() }),
  z.any(),
);

const Offset = styled("div")(({ theme }) => theme.mixins.toolbar);

export default function App() {
  return (
    <WebSocketProvider url="ws://127.0.0.1:30180">
      <ExportRequestProvider>
        <RefreshProvider>
          <Container maxWidth="xl">
            <AppBar color="default" position="fixed">
              <Toolbar variant="dense">
                <Box sx={{ flexGrow: 1 }} />
                <RefreshInput />
                <ConnectionStatus />
              </Toolbar>
            </AppBar>
            <Offset />
            <Grid
              container
              spacing={0}
              columns={{ xs: 1, md: 2, lg: 3, xl: 4 }}
              sx={{ justifyContent: "center", alignItems: "center" }}
            >
              {/* Heap Size */}
              <Grid size={1}>
                <Plot
                  name="heapSize"
                  validate={(attrs) => z.validate(heapSizeAttrs, attrs)}
                  labelFor={(attrs) =>
                    `${attrs.name} / cap ${attrs.capNo}`
                  }
                  seriesFor={(label, index) => ({
                    label,
                    stroke: colorCycle[index],
                    spanGaps: true,
                  })}
                  options={{
                    title: "Heap Size",
                    width: 600,
                    height: 400,
                    scales: {
                      x: { time: true },
                      y: { time: false },
                    },
                  }}
                />
              </Grid>
              {/* GC Stats */}
              <Grid size={1}>
                <Plot
                  name="gcCopied"
                  validate={(attrs) => z.validate(gcStatsAttrs, attrs)}
                  labelFor={(attrs) =>
                    `cap ${attrs.capNo} / gen ${attrs.gen}`
                  }
                  seriesFor={(label, index) => ({
                    label,
                    stroke: colorCycle[index],
                    spanGaps: true,
                  })}
                  options={{
                    title: "GC Copied",
                    width: 600,
                    height: 400,
                    scales: {
                      x: { time: true },
                      y: { time: false },
                    },
                  }}
                />
              </Grid>
              <Grid size={1}>
                <Plot
                  name="gcSlop"
                  validate={(attrs) => z.validate(gcStatsAttrs, attrs)}
                  labelFor={(attrs) =>
                    `cap ${attrs.capNo} / gen ${attrs.gen}`
                  }
                  seriesFor={(label, index) => ({
                    label,
                    stroke: colorCycle[index],
                    spanGaps: true,
                  })}
                  options={{
                    title: "GC Slop",
                    width: 600,
                    height: 400,
                    scales: {
                      x: { time: true },
                      y: { time: false },
                    },
                  }}
                />
              </Grid>
              <Grid size={1}>
                <Plot
                  name="gcFragmentation"
                  validate={(attrs) => z.validate(gcStatsAttrs, attrs)}
                  labelFor={(attrs) =>
                    `cap ${attrs.capNo} / gen ${attrs.gen}`
                  }
                  seriesFor={(label, index) => ({
                    label,
                    stroke: colorCycle[index],
                    spanGaps: true,
                  })}
                  options={{
                    title: "GC Fragmentation",
                    width: 600,
                    height: 400,
                    scales: {
                      x: { time: true },
                      y: { time: false },
                    },
                  }}
                />
              </Grid>
            </Grid>
          </Container>
        </RefreshProvider>
      </ExportRequestProvider>
    </WebSocketProvider>
  );
}
