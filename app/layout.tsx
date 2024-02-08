import type { Metadata } from "next";
import "./globals.css";
import '@radix-ui/themes/styles.css'
import { Theme } from "@radix-ui/themes";

export const metadata: Metadata = {
  title: "19tools",
  description: "Tools for life",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en" className="min-h-svh flex">
      <body className="flex-1 flex">
        <Theme className="flex-1 flex justify-center items-center">{children}</Theme>
      </body>
    </html>
  );
}
