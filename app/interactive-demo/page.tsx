'use client';

import React from 'react';
import PyodideTerminal from '@/components/interactive/PyodideTerminal';

export default function InteractiveDemoPage() {
  return (
    <div className="min-h-screen pt-24 pb-16 px-4">
      <div className="container mx-auto max-w-5xl">
        <h1 className="text-4xl font-bold mb-6 text-white neon-glow">Interactive Gravity Playground</h1>
        <p className="text-white/80 text-lg mb-8">
          Experiment with gravity models by modifying parameters and seeing real-time results.
          This interactive terminal lets you explore how different values affect galaxy rotation curves.
        </p>
        
        <PyodideTerminal />
        
        <div className="mt-10 bg-black/60 border border-dark-pink/20 p-6 rounded-md">
          <h2 className="text-2xl font-semibold text-white mb-4">How to Use</h2>
          <ul className="list-disc pl-6 text-white/80 space-y-3">
            <li>
              <strong className="text-dark-pink">Modify Parameters:</strong> Try changing the 
              galaxy mass (<code>M_galaxy</code>), the acceleration scale (<code>a0</code>), 
              or the radius range to see how they affect the rotation curves.
            </li>
            <li>
              <strong className="text-dark-pink">Run the Code:</strong> Click the "Run Code" button 
              to execute your modifications and see the updated visualization.
            </li>
            <li>
              <strong className="text-dark-pink">Compare Models:</strong> The graph shows both 
              standard Newtonian gravity (which would require dark matter to explain observations) 
              and Emergent Gravity (which attempts to explain observations without dark matter).
            </li>
            <li>
              <strong className="text-dark-pink">Scientific Implications:</strong> A flat rotation 
              curve at large radii is what we observe in real galaxies. Which model better matches 
              this observation? What does this tell us about dark matter?
            </li>
          </ul>
        </div>
      </div>
    </div>
  );
} 