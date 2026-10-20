"use client";

import WebSocketProvider from "@/components/WebSocketProvider";
import ExportRequestProvider from "@/components/ExportRequestProvider";
import Plot from "@/components/Plot";
import RefreshProvider from "@/components/RefreshProvider";
import RefreshInput from "@/components/RefreshInput";
import Grid from "@mui/material/Grid";
import Container from "@mui/material/Container";
import ConnectionStatus from "@/components/ConnectionStatus";
import AppBar from "@mui/material/AppBar";
import Toolbar from "@mui/material/Toolbar";
import Box from "@mui/material/Box";
import { styled } from "@mui/material";

const width = 600;
const height = 400;

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
          </Container>
        </RefreshProvider>
      </ExportRequestProvider>
    </WebSocketProvider>
  );
}
