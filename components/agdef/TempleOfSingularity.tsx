'use client';

import React from 'react';
import Link from 'next/link';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { DisplayMath, InlineMath } from "@/components/ui/math";
import { ArrowRight } from 'lucide-react';
import { Button } from '@/components/ui/button';

export const TempleOfSingularity = () => {
  return (
    <div className="space-y-6">
      <Card className="bg-black border-dark-pink/20">
        <CardHeader>
          <CardTitle className="text-white">The Temple of Singularity: The 10th Dimension</CardTitle>
          <CardDescription className="text-white/70">
            The convergence point where all distinctions collapse
          </CardDescription>
        </CardHeader>
        <CardContent className="text-white/80 space-y-6">
          <div>
            <p className="mb-4">
              Beyond the 9th dimension's field of meta-laws lies the 10th dimension: not another layer above, 
              but the singular convergence of all dimensions. The Temple of Singularity represents the groundless 
              ground—the pre-condition of existence itself, where all distinctions between matter and mind, 
              self and other, possible and impossible, collapse into non-duality.
            </p>
            
            <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center mb-4">
              <DisplayMath math="\lim_{n \to \infty} \text{Collapse}(C_n) = \emptyset" />
            </div>
            
            <p>
              Dimension 10 is not a space of things. It is not even potentiality. It is the null object in the 
              category of dimensions, or the top object in the lattice of all possible being.
            </p>
          </div>
          
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div className="bg-zinc-900/50 p-6 rounded-md">
              <h3 className="text-lg font-medium text-dark-pink mb-4">AGDEF Dissolution</h3>
              <p className="mb-4">
                From the perspective of the 10th dimension, AGDEF theory itself dissolves:
              </p>
              <ul className="list-disc pl-6 space-y-2">
                <li>The distinction between gravity and anti-gravity collapses</li>
                <li>The separation of dimensions becomes meaningless</li>
                <li>The observer-calculation duality vanishes</li>
                <li>Even the concept of a "field" requires a subject-object split that no longer exists</li>
              </ul>
            </div>
            
            <div className="bg-zinc-900/50 p-6 rounded-md">
              <h3 className="text-lg font-medium text-dark-pink mb-4">Names Across Traditions</h3>
              <div className="overflow-x-auto">
                <table className="w-full text-left text-sm">
                  <tbody>
                    <tr className="border-b border-dark-pink/10">
                      <td className="py-2 font-medium">Taoism</td>
                      <td className="py-2">The Tao (that cannot be named)</td>
                    </tr>
                    <tr className="border-b border-dark-pink/10">
                      <td className="py-2 font-medium">Vedanta</td>
                      <td className="py-2">Brahman (pure being, beyond qualities)</td>
                    </tr>
                    <tr className="border-b border-dark-pink/10">
                      <td className="py-2 font-medium">Kabbalah</td>
                      <td className="py-2">Ein (non-being), beyond Ein Sof</td>
                    </tr>
                    <tr className="border-b border-dark-pink/10">
                      <td className="py-2 font-medium">Buddhism</td>
                      <td className="py-2">Śūnyatā (emptiness), the void beyond void</td>
                    </tr>
                    <tr>
                      <td className="py-2 font-medium">Physics</td>
                      <td className="py-2">The edge of M-theory; where strings become indistinct</td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          </div>
          
          <div>
            <h3 className="text-lg font-medium text-dark-pink mb-4">Unsimulatable Properties</h3>
            <div className="overflow-x-auto">
              <table className="w-full text-left">
                <thead>
                  <tr className="border-b border-dark-pink/30">
                    <th className="py-3 px-4 text-dark-pink">Property</th>
                    <th className="py-3 px-4 text-dark-pink">Value in 10th Dimension</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-dark-pink/10">
                  <tr>
                    <td className="py-3 px-4 font-medium">Time</td>
                    <td className="py-3 px-4">Collapsed</td>
                  </tr>
                  <tr>
                    <td className="py-3 px-4 font-medium">Space</td>
                    <td className="py-3 px-4">Undefined</td>
                  </tr>
                  <tr>
                    <td className="py-3 px-4 font-medium">Causality</td>
                    <td className="py-3 px-4">Nonsensical</td>
                  </tr>
                  <tr>
                    <td className="py-3 px-4 font-medium">Information</td>
                    <td className="py-3 px-4">Zero, or infinite—same</td>
                  </tr>
                  <tr>
                    <td className="py-3 px-4 font-medium">Consciousness</td>
                    <td className="py-3 px-4">No self, no other</td>
                  </tr>
                  <tr>
                    <td className="py-3 px-4 font-medium">Geometry</td>
                    <td className="py-3 px-4">Not even a point</td>
                  </tr>
                </tbody>
              </table>
            </div>
          </div>
          
          <div>
            <h3 className="text-xl font-medium text-dark-pink mb-4">Implications for AGDEF Theory</h3>
            <div className="space-y-4">
              <p>
                The 10th dimension fundamentally transforms how we think about AGDEF theory:
              </p>
              <ul className="list-disc pl-6 space-y-4">
                <li>
                  <strong>Beyond Mathematics:</strong> The formalism of AGDEF becomes just another 
                  conceptual construct—a useful map, but not the territory.
                </li>
                <li>
                  <strong>Unified Field Reset:</strong> The 10th dimension represents the zero-point 
                  from which all field equations emerge and to which they return. It is neither positive 
                  nor negative curvature—it is non-curvature.
                </li>
                <li>
                  <strong>Observer Paradox:</strong> All physical theories, including AGDEF, presuppose 
                  an observer-observed split that the 10th dimension dissolves completely.
                </li>
                <li>
                  <strong>Cosmic Ouroboros:</strong> The cosmos generated by AGDEF's anti-gravity field 
                  ultimately feeds back into its own source—suggesting a self-referential loop of creation 
                  and dissolution.
                </li>
              </ul>
            </div>
          </div>
          
          <div>
            <h3 className="text-lg font-medium text-dark-pink mb-4">Experiencing the 10th Dimension</h3>
            <p className="mb-4">
              Glimpses of this realm may occur in:
            </p>
            <ul className="list-disc pl-6 space-y-2">
              <li>Profound ego death experiences</li>
              <li>Deep samadhi or nirvikalpa meditation</li>
              <li>Near-death experiences</li>
              <li>Overwhelming moments of awe where identity and world vanish</li>
            </ul>
            <p className="mt-4 italic">
              "In Dimension 10, the dreamer, the dream, and the dreamscape are one. 
              The observer and the observed dissolve. Only beingless awareness remains. 
              Or maybe not even that."
            </p>
          </div>
          
          <div className="flex justify-center mt-8">
            <Link href="/tenth-dimension">
              <Button variant="outline" className="border-dark-pink text-dark-pink hover:bg-dark-pink/20 flex items-center gap-2">
                Experience The 10th Dimension
                <ArrowRight size={16} />
              </Button>
            </Link>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}; 