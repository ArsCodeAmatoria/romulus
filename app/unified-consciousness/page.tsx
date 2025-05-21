'use client';

import { UnifiedConsciousness } from "@/components/agdef/UnifiedConsciousness";

export default function UnifiedConsciousnessPage() {
  return (
    <div className="min-h-screen pt-24 pb-16 px-4">
      <div className="container mx-auto max-w-4xl">
        <h1 className="text-4xl font-bold mb-6 text-white neon-glow">Unified Consciousness Theory</h1>
        <p className="text-white/80 text-lg mb-8">
          A mathematical formalization of consciousness as the fundamental reality, using category theory, linear algebra, and Haskell.
        </p>
        
        <UnifiedConsciousness />
      </div>
    </div>
  );
} 