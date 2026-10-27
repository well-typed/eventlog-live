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

export interface ExportRequestContext {
  lastExportRequest: ViewerExportRequest | null;
}

export const ExportRequestContext = createContext<ExportRequestContext>({
  lastExportRequest: null,
});

export interface TelemetryProviderProps {}

export default function TelemetryProvider({
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
    <ExportRequestContext.Provider value={{ lastExportRequest }}>
      {children}
    </ExportRequestContext.Provider>
  );
}
