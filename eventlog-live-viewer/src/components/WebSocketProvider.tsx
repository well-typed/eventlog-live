"use client";

import { createContext, PropsWithChildren } from "react";
import useWebSocket, { ReadyState } from "react-use-websocket";

export interface WebSocketContext {
  lastMessage: MessageEvent<any> | null;
  readyState: ReadyState;
}

export const WebSocketContext = createContext<WebSocketContext>({
  lastMessage: null,
  readyState: ReadyState.UNINSTANTIATED,
});

interface WebSocketProviderProps {
  url: string;
}

function WebSocketProvider({
  url,
  children,
}: PropsWithChildren<WebSocketProviderProps>) {
  const { lastMessage, readyState } = useWebSocket(url, {
    reconnectAttempts: 10,
    reconnectInterval: 30000,
  });
  return (
    <WebSocketContext.Provider value={{ lastMessage, readyState }}>
      {children}
    </WebSocketContext.Provider>
  );
}

export default WebSocketProvider;
