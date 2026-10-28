import WebSocketProvider from "@/components/WebSocketProvider";
import ExportRequestProvider from "@/components/ExportRequestProvider";
import RefreshProvider from "@/components/RefreshProvider";
import RefreshSwitch from "@/components/RefreshSwitch";
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
import "uplot/dist/uPlot.min.css";
import resolveWebSocketUrl from "@/util/resolveWebSocketUrl";

export default function App() {
  return (
    <WebSocketProvider url={resolveWebSocketUrl()}>
      <ExportRequestProvider>
        <RefreshProvider>
          <Container maxWidth="xl">
            <AppBar color="default" position="fixed">
              <Toolbar variant="dense">
                <Typography variant="h4">
                  <EventlogLiveIcon
                    sx={[
                      {
                        fontSize: "1em",
                        marginRight: 1,
                        verticalAlign: "-10%",
                      },
                    ]}
                  />
                  Eventlog Live
                </Typography>
                <Box sx={{ flexGrow: 1 }} />
                <RefreshSwitch />
                <ConnectionStatus />
              </Toolbar>
            </AppBar>
            <Offset />
            <Grid
              container
              spacing={0}
              columns={{ xs: 1, md: 2 }}
              sx={{ justifyContent: "center", alignItems: "start" }}
            >
              {/* Heap Profile */}
              <Grid size={1}>
                <HeapProfSamplePlot />
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
                  <HeapSizePlot />
                </Grid>
                {/* GC Stats */}
                <Grid size={1}>
                  <GcStats.GcCopiedPlot />
                </Grid>
                <Grid size={1}>
                  <GcStats.GcSlopPlot />
                </Grid>
                <Grid size={1}>
                  <GcStats.GcFragmentationPlot />
                </Grid>
              </Grid>
            </Grid>
          </Container>
        </RefreshProvider>
      </ExportRequestProvider>
    </WebSocketProvider>
  );
}
