import type { Metadata } from "next";
import "./globals.css";
import Navigation from "@/components/Navigation";

// Site created by Ars Code Amatoria (https://github.com/ArsCodeAmatoria)

export const metadata: Metadata = {
  title: "Romulus - Dark Matter Science",
  description: "Exploring Dark Matter through Modified Gravity theories",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en" className="dark">
      <body className="min-h-screen bg-dark-black antialiased">
        <Navigation />
        {children}
      </body>
    </html>
  );
}
