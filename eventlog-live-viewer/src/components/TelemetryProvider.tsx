"use client";

import type { ViewerExportRequest } from "@/util/z";
import {
  createContext,
  PropsWithChildren,
  useContext,
  useEffect,
  useState,
} from "react";
import { WebSocketContext } from "./WebSocketProvider";
import * as z from "../util/z";

export interface TelemetryContext {
  lastExportRequest: ViewerExportRequest | null;
}

export const TelemetryContext = createContext<TelemetryContext>({
  lastExportRequest: null,
});

interface TelemetryProviderProps {}

function TelemetryProvider({
  children,
}: PropsWithChildren<TelemetryProviderProps>) {
  const ws = useContext(WebSocketContext);
  const [lastExportRequest, setLastExportRequest] =
    useState<ViewerExportRequest | null>(null);

  useEffect(() => {
    const lastMessage = ws.lastMessage;
    if (lastMessage !== null) {
      try {
        const lastJsonMessage = JSON.parse(lastMessage.data);
        setLastExportRequest(z.decode(z.viewerExportRequest, lastJsonMessage));
      } catch (e) {
        console.error(e);
      }
    }
  }, [ws.lastMessage]);

  return (
    <TelemetryContext.Provider value={{ lastExportRequest }}>
      {children}
    </TelemetryContext.Provider>
  );
}

export default TelemetryProvider;
