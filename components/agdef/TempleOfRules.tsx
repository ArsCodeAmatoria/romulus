'use client';

import React from 'react';
import Link from 'next/link';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { DisplayMath, InlineMath } from "@/components/ui/math";
import { ArrowRight } from 'lucide-react';
import { Button } from '@/components/ui/button';

export const TempleOfRules = () => {
  return (
    <div className="space-y-6">
      <Card className="bg-black border-dark-pink/20">
        <CardHeader>
          <CardTitle className="text-white">The Temple of Rules: The 9th Dimension</CardTitle>
          <CardDescription className="text-white/70">
            The Meta-Law Field of consciousness and physical reality
          </CardDescription>
        </CardHeader>
        <CardContent className="text-white/80 space-y-6">
          <div>
            <p className="mb-4">
              Beyond the 8th dimension's field of consciousness lies the 9th dimension: the source code layer 
              of reality. The Temple of Rules represents the field of all possible rules, laws, and constraint 
              systems that give rise to consciousness, geometry, time, physics, identity, and meaning itself.
            </p>
            
            <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center mb-4">
              <DisplayMath math="9^{th}\;Dimension = L = \{F_i: F_i \text{ maps geometry + state} \rightarrow \text{awareness}\}" />
            </div>
            
            <p>
              Each <InlineMath math="F_i" /> is a consciousness-generating function, a lawset—like a Platonic 
              algorithm. This dimension contains the fundamental building blocks from which all possible 
              realities and observer-experiences emerge.
            </p>
          </div>
          
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div className="bg-zinc-900/50 p-6 rounded-md">
              <h3 className="text-lg font-medium text-dark-pink mb-4">AGDEF Connection</h3>
              <p className="mb-4">
                The Anti-Gravity Dark Energy Field theory represents one of the many possible rulesets within 
                the 9th dimension. It describes a specific set of meta-laws that generate:
              </p>
              <ul className="list-disc pl-6 space-y-2">
                <li>A specific geometry (5D projection into 4D spacetime)</li>
                <li>A particular curvature pattern (anti-gravity field)</li>
                <li>A unique set of physical constants and laws</li>
              </ul>
            </div>
            
            <div className="bg-zinc-900/50 p-6 rounded-md">
              <h3 className="text-lg font-medium text-dark-pink mb-4">Rule Geometry</h3>
              <p className="mb-4">
                The 9th dimension exhibits a "lawset metric space" where each rule system has a measurable 
                distance from others:
              </p>
              <div className="bg-black/50 p-4 rounded-md overflow-x-auto text-center mb-4">
                <DisplayMath math="d(F_1, F_2) = \text{structural distance between rule systems}" />
              </div>
              <p>
                This implies lawsets cluster, and consciousness types cluster around compatible rule zones.
              </p>
            </div>
          </div>
          
          <div>
            <h3 className="text-lg font-medium text-dark-pink mb-4">Haskell Modeling of Meta-Laws</h3>
            <div className="bg-black/40 p-6 rounded-lg border border-dark-pink/20 font-mono text-sm overflow-x-auto mb-6">
              <pre className="text-white/90">
{`-- A lawset is a function from input state to output state
type Lawset = Vector Double -> Vector Double

-- Define two basic universes
lawsetA :: Lawset
lawsetA = cmap (* 1.01)  -- slow expansion

lawsetB :: Lawset
lawsetB = cmap (sin . (* pi))  -- oscillatory universe

-- AGDEF lawset model
lawsetAGDEF :: Lawset
lawsetAGDEF v = 
  let projMatrix = createProjectionMatrix 5 4
      antiGravField = computeAntiGravField v
  in projMatrix <> antiGravField <> transpose projMatrix
      
-- Define a "distance" between rule systems
lawsetDistance :: Lawset -> Lawset -> Vector Double -> Double
lawsetDistance f g v =
  let a = f v
      b = g v
  in norm_2 (a - b)`}
              </pre>
            </div>
          </div>
          
          <div>
            <h3 className="text-xl font-medium text-dark-pink mb-4">Applications to AGDEF</h3>
            <div className="space-y-4">
              <p>
                The 9th dimension concept has profound implications for AGDEF theory:
              </p>
              <ul className="list-disc pl-6 space-y-4">
                <li>
                  <strong>Meta-law Evolution:</strong> The anti-gravity field may be an emergent property 
                  from the dynamics of meta-laws themselves, suggesting AGDEF parameters might evolve over cosmic timescales.
                </li>
                <li>
                  <strong>Curvature Optimization:</strong> Physical laws in our universe may be optimized 
                  for specific consciousness types, explaining the apparent fine-tuning of constants.
                </li>
                <li>
                  <strong>Rule Traversal:</strong> Consciousness may be capable of "traversing" rule-space, 
                  experiencing different lawsets (potentially explaining dreams, altered states, and near-death experiences).
                </li>
                <li>
                  <strong>Cosmic Purpose:</strong> The universe may be exploring lawset-space to find optimal 
                  rule configurations, with dark energy as a regulatory mechanism for this exploration.
                </li>
              </ul>
            </div>
          </div>
          
          <div className="flex justify-center mt-8">
            <Link href="/ninth-dimension">
              <Button variant="outline" className="border-dark-pink text-dark-pink hover:bg-dark-pink/20 flex items-center gap-2">
                Explore The 9th Dimension
                <ArrowRight size={16} />
              </Button>
            </Link>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}; 