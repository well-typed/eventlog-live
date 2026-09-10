"use client";

import WebSocketProvider from "@/components/WebSocketProvider";
import TelemetryProvider from "@/components/TelemetryProvider";
import Plot from "@/components/Plot";
import RefreshProvider from "@/components/RefreshProvider";
import RefreshInput from "@/components/RefreshInput";
import FormGroup from "@mui/material/FormGroup";
import FormControlLabel from "@mui/material/FormControlLabel";
import Grid from "@mui/material/Grid";

const width = 600;
const height = 400;

export default function App() {
  return (
    <WebSocketProvider url="ws://127.0.0.1:30180">
      <TelemetryProvider>
        <RefreshProvider>
          <FormGroup>
            <FormControlLabel control={<RefreshInput />} label="Refresh" />
          </FormGroup>
          <Grid
            container
            spacing={2}
            columns={{ xs: 1, md: 2, lg: 3, xl: 4 }}
            sx={{ justifyContent: "center", alignItems: "center" }}
          >
            <Grid size={1}>
              <Plot
                name="heapSize"
                legend={["heapSize", "blocksSize", "heapLive"]}
                options={{
                  title: "Heap Size",
                  id: "heapSizePlot",
                  width,
                  height,
                  scales: {
                    x: {
                      time: true,
                    },
                    y: {
                      time: false,
                    },
                  },
                  axes: [{}],
                  series: [
                    {},
                    {
                      label: "HeapSize",
                      stroke: "red",
                      spanGaps: true,
                    },
                    {
                      label: "BlocksSize",
                      stroke: "orange",
                      spanGaps: true,
                    },
                    {
                      label: "HeapLive",
                      stroke: "blue",
                      spanGaps: true,
                    },
                  ],
                }}
              />
            </Grid>
            <Grid size={1}>
              <Plot
                name="gcCopied"
                legend={["gcCopied"]}
                options={{
                  title: "GC Copied",
                  id: "gcCopiedPlot",
                  width,
                  height,
                  scales: {
                    x: {
                      time: true,
                    },
                    y: {
                      time: false,
                    },
                  },
                  axes: [{}],
                  series: [
                    {},
                    {
                      label: "GC Copied",
                      stroke: "red",
                      spanGaps: true,
                    },
                  ],
                }}
              />
            </Grid>
            <Grid size={1}>
              <Plot
                name="gcSlop"
                legend={["gcSlop"]}
                options={{
                  title: "GC Slop",
                  id: "gcSlopPlot",
                  width,
                  height,
                  scales: {
                    x: {
                      time: true,
                    },
                    y: {
                      time: false,
                    },
                  },
                  axes: [{}],
                  series: [
                    {},
                    {
                      label: "GC Slop",
                      stroke: "red",
                      spanGaps: true,
                    },
                  ],
                }}
              />
            </Grid>
            <Grid size={1}>
              <Plot
                name="gcFragmentation"
                legend={["gcFragmentation"]}
                options={{
                  title: "GC Fragmentation",
                  id: "gcFragmentationPlot",
                  width,
                  height,
                  scales: {
                    x: {
                      time: true,
                    },
                    y: {
                      time: false,
                    },
                  },
                  axes: [{}],
                  series: [
                    {},
                    {
                      label: "GC Fragmentation",
                      stroke: "red",
                      spanGaps: true,
                    },
                  ],
                }}
              />
            </Grid>
          </Grid>
        </RefreshProvider>
      </TelemetryProvider>
    </WebSocketProvider>
  );
}
