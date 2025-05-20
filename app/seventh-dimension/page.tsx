'use client';

import { SeventhDimension } from "@/components/agdef/SeventhDimension";

export default function SeventhDimensionPage() {
  return (
    <div className="min-h-screen pt-24 pb-16 px-4">
      <div className="container mx-auto max-w-4xl">
        <h1 className="text-4xl font-bold mb-6 text-white neon-glow">The 7th Dimension Theory</h1>
        <p className="text-white/80 text-lg mb-8">
          Exploring the configuration space of possible universes in the AGDEF theoretical framework.
        </p>
        
        <SeventhDimension />
      </div>
    </div>
  );
} 