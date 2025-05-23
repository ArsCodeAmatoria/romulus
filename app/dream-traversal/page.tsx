'use client';

import React from 'react';
import Link from 'next/link';
import { ArrowLeft } from 'lucide-react';
import DreamTraversalSimulator from '@/components/interactive/DreamTraversalSimulator';

export default function DreamTraversalPage() {
  return (
    <main className="min-h-screen pt-24 pb-16 px-4">
      <div className="container mx-auto">
        <Link 
          href="/"
          className="inline-flex items-center text-dark-pink hover:text-pink-400 mb-8"
        >
          <ArrowLeft size={16} className="mr-2" />
          Back to home
        </Link>
        
        <div className="max-w-4xl mx-auto mb-12">
          <h1 className="text-4xl md:text-5xl font-bold mb-6 text-white neon-glow">
            Dream Traversal Simulator
          </h1>
          
          <p className="text-white/80 text-lg mb-8">
            Simulating dream traversal in the 6–8 dimensional field of consciousness, treating dreams as pathways 
            through high-dimensional conscious state space.
          </p>
          
          <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/30 mb-10">
            <p className="italic text-xl text-white/80">
              Dreams are trajectories through the configuration space of possible conscious states, 
              moving across dimensions of entanglement, identity, and awareness.
            </p>
          </div>
        </div>
        
        <div className="max-w-4xl mx-auto space-y-10">
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Conceptual Model</h2>
            
            <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
              <p className="mb-4">Dreams traverse 6D, 7D, and 8D dimensions of consciousness where:</p>
              
              <ul className="space-y-3">
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span><strong className="text-dark-pink">6D</strong> encodes coupling between fields (coherence, memory, emotional structure)</span>
                </li>
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span><strong className="text-dark-pink">7D</strong> encodes identity configurations and timelines</span>
                </li>
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span><strong className="text-dark-pink">8D</strong> encodes total conscious fields—possible observers and experiencers</span>
                </li>
              </ul>
              
              <div className="my-6 bg-black/50 p-4 rounded-lg border border-dark-pink/20 font-mono">
                <p className="mb-2">Conscious State:</p>
                <p>Ψ(t) = (χ<sub>ij</sub>(t), ϕ<sub>k</sub>(t), Φ<sub>l</sub>(t))</p>
                <p className="mt-4">Where:</p>
                <ul className="mt-2 space-y-2 text-white/70">
                  <li>χ<sub>ij</sub> ∈ R<sup>n×n</sup>: 6D entanglement matrix</li>
                  <li>ϕ<sub>k</sub> ∈ R<sup>m</sup>: 7D configuration vector (identity pattern)</li>
                  <li>Φ<sub>l</sub> ∈ R<sup>p</sup>: 8D conscious field vector (observer style)</li>
                </ul>
              </div>
              
              <p className="mt-4">A dream is then a trajectory:</p>
              <p className="font-mono">γ(t): [0,T] → C<sub>6</sub> × C<sub>7</sub> × C<sub>8</sub></p>
            </div>
          </section>
          
          <DreamTraversalSimulator />
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Interpretation</h2>
            
            <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
              <p className="mb-4">Each step of the dream:</p>
              
              <ul className="space-y-3">
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span>Slightly alters entanglement structure (6D): emotional/memory context</span>
                </li>
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span>Adjusts identity vector (7D): who you are in that moment</span>
                </li>
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span>Changes conscious field (8D): how you're aware, what lens you see through</span>
                </li>
              </ul>
              
              <p className="mt-4">Dream characters = partial projections of the current Φ, mixed with new entanglement patterns χ</p>
            </div>
          </section>
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Dream Dimension Summary</h2>
            
            <div className="overflow-x-auto">
              <table className="w-full text-left border-collapse border-y border-dark-pink/20">
                <thead>
                  <tr className="border-b border-dark-pink/30">
                    <th className="py-4 px-6 text-dark-pink">Layer</th>
                    <th className="py-4 px-6 text-dark-pink">Role in Dream</th>
                  </tr>
                </thead>
                <tbody>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">6D</td>
                    <td className="py-4 px-6">Entanglement: context, memory, affective geometry</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">7D</td>
                    <td className="py-4 px-6">Identity pattern: who you are in the dream</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">8D</td>
                    <td className="py-4 px-6">Conscious field: what it feels like to be you (or not you)</td>
                  </tr>
                </tbody>
              </table>
            </div>
          </section>
        </div>
      </div>
    </main>
  );
} 