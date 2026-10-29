import WebSocketProvider from "@/components/WebSocketProvider";
import ExportRequestProvider from "@/components/ExportRequestProvider";
import RefreshProvider from "@/components/refresh/RefreshProvider";
import RefreshSwitch from "@/components/refresh/RefreshSwitch";
import Grid from "@mui/material/Grid";
import Container from "@mui/material/Container";
import ConnectionStatus from "@/components/ConnectionStatus";
import AppBar from "@mui/material/AppBar";
import Toolbar from "@mui/material/Toolbar";
import Box from "@mui/material/Box";
import { HeapProfSamplePlot } from "@/components/plots/HeapProfSample";
import { HeapSizePlot } from "@/components/plots/HeapSize";
import * as GcStats from "@/components/plots/GcStats";
import { Offset } from "@/components/Offset";
import EventlogLiveIcon from "@/components/EventlogLiveIcon";
import Typography from "@mui/material/Typography";
import resolveWebSocketUrl from "@/util/resolveWebSocketUrl";
import uPlot from "uplot";
import "uplot/dist/uPlot.min.css";
import "@/style/uPlot.css";
import CursorSyncProvider from "./cursorSync/CursorSyncProvider";
import CursorSyncSwitch from "./cursorSync/CursorSyncSwitch";
import ServiceTabs from "./ServiceTabs";

export default function App() {
  // Right-Stacked Legend:
  const hooks: uPlot["hooks"] = {
    init: [
      (self: uPlot) => {
        self.root.classList.add("u-stacked-legend");
      },
    ],
  };

  return (
    <WebSocketProvider url={resolveWebSocketUrl()}>
      <ExportRequestProvider>
        <RefreshProvider>
          <CursorSyncProvider>
            <Container maxWidth="xl">
              <AppBar color="default" position="fixed">
                <Toolbar variant="dense">
                  <EventlogLiveIcon
                    sx={[
                      {
                        fontSize: "2.35em",
                        marginRight: 1,
                        marginBottom: "0.22em",
                      },
                    ]}
                  />
                  <Typography
                    variant="h4"
                    sx={{ display: { xs: "none", sm: "inherit" } }}
                  >
                    Eventlog&nbsp;Live
                  </Typography>
                  <Box sx={{ flexGrow: 1 }} />
                  <CursorSyncSwitch />
                  <RefreshSwitch />
                  <ConnectionStatus />
                </Toolbar>
              </AppBar>
              <Offset />
              <ServiceTabs>
                <Grid
                  container
                  spacing={0}
                  columns={{ xs: 1, md: 2 }}
                  sx={{ justifyContent: "center", alignItems: "start" }}
                >
                  {/* Heap Profile */}
                  <Grid size={1}>
                    <HeapProfSamplePlot options={{ hooks }} />
                  </Grid>

                  <Grid
                    size={1}
                    container
                    spacing={0}
                    columns={{ xs: 1, xl: 2 }}
                    sx={{ justifyContent: "center", alignItems: "start" }}
                  >
                    {/* Heap Size */}
                    <Grid size={1}>
                      <HeapSizePlot options={{ hooks }} />
                    </Grid>
                    {/* GC Stats */}
                    <Grid size={1}>
                      <GcStats.GcCopiedPlot options={{ hooks }} />
                    </Grid>
                    <Grid size={1}>
                      <GcStats.GcSlopPlot options={{ hooks }} />
                    </Grid>
                    <Grid size={1}>
                      <GcStats.GcFragmentationPlot options={{ hooks }} />
                    </Grid>
                  </Grid>
                </Grid>
              </ServiceTabs>
            </Container>
          </CursorSyncProvider>
        </RefreshProvider>
      </ExportRequestProvider>
    </WebSocketProvider>
  );
}
