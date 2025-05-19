'use client';

import { usePathname } from "next/navigation";
import Link from "next/link";

interface DocsLayoutProps {
  children: React.ReactNode;
}

export default function DocsLayout({ children }: DocsLayoutProps) {
  const pathname = usePathname();
  
  const isDetailPage = pathname !== '/docs';
  
  return (
    <div className="min-h-screen">
      {isDetailPage && (
        <div className="bg-black/50 py-3 px-4 border-b border-dark-pink/20 mt-20">
          <div className="container mx-auto">
            <Link href="/docs" className="text-dark-pink hover:underline text-sm">&larr; Back to Theory Explorer</Link>
          </div>
        </div>
      )}
      {children}
    </div>
  );
} 