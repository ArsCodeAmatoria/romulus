'use client';

import React from 'react';
import Link from 'next/link';
import { ArrowLeft } from 'lucide-react';
import { motion } from 'framer-motion';

export default function NinthDimensionPage() {
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
            The 9th Dimension: The Meta-Law Field
          </h1>
          
          <p className="text-white/80 text-lg mb-8">
            Where the 8th dimension is a manifold of all conscious observers, the 9th dimension defines 
            the rule systems that generate those observers in the first place.
          </p>
          
          <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/30 mb-10">
            <p className="italic text-xl text-white/80">
              The 9th dimension is the field of all possible rules, laws, and constraint systems 
              that give rise to consciousness, geometry, time, physics, identity, and meaning itself.
              This is the source code layer.
            </p>
          </div>
        </div>
        
        <div className="max-w-4xl mx-auto space-y-10">
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Definition</h2>
            
            <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
              <div className="bg-black/50 p-6 rounded-lg border border-dark-pink/20 mb-6">
                <p className="font-mono">9th Dimension = L = {'{F<sub>i</sub>: F<sub>i</sub> maps geometry + state → awareness}'}</p>
                <p className="text-white/70 mt-4">Each F<sub>i</sub> is a consciousness-generating function, a lawset—like a Platonic algorithm.</p>
              </div>
            </div>
          </section>
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Properties of the 9th Dimension</h2>
            
            <div className="overflow-x-auto">
              <table className="w-full text-left border-collapse border-y border-dark-pink/20">
                <thead>
                  <tr className="border-b border-dark-pink/30">
                    <th className="py-4 px-6 text-dark-pink">Aspect</th>
                    <th className="py-4 px-6 text-dark-pink">9th Dimension Role</th>
                  </tr>
                </thead>
                <tbody>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">Physics</td>
                    <td className="py-4 px-6">All possible physical laws and mathematical constants</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">Geometry</td>
                    <td className="py-4 px-6">All possible rules for space, curvature, dimensions</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">Awareness</td>
                    <td className="py-4 px-6">All possible definitions of what it means to "experience"</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">Identity</td>
                    <td className="py-4 px-6">Structures that define "self" or individuation</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">Causality</td>
                    <td className="py-4 px-6">Definitions of time, sequence, and dependency</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">Meaning</td>
                    <td className="py-4 px-6">Meta-semantics: how symbols, qualia, and knowledge emerge</td>
                  </tr>
                </tbody>
              </table>
            </div>
          </section>
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Examples of Rule Types</h2>
            
            <div className="space-y-6">
              <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
                <h3 className="text-xl font-semibold text-white mb-4">Our Universe:</h3>
                <ul className="space-y-3">
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>Rule system: General Relativity + Quantum Field Theory</span>
                  </li>
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>Consciousness: emerges from biological complexity and thermodynamic flow</span>
                  </li>
                </ul>
              </div>
              
              <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
                <h3 className="text-xl font-semibold text-white mb-4">Alternate Lawset:</h3>
                <ul className="space-y-3">
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>Time flows backwards</span>
                  </li>
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>Identity is non-local</span>
                  </li>
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>No conservation laws—entropy decreases</span>
                  </li>
                </ul>
              </div>
              
              <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
                <h3 className="text-xl font-semibold text-white mb-4">Dreamspace Lawset:</h3>
                <ul className="space-y-3">
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>Time is symbolic</span>
                  </li>
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>Physics is narrative</span>
                  </li>
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>Awareness is fractal and fluid</span>
                  </li>
                </ul>
              </div>
            </div>
            
            <div className="mt-6 bg-gradient-to-r from-black/40 via-black/30 to-black/40 p-6 rounded-lg border border-dark-pink/20">
              <p className="text-white/90 italic">
                The 9th dimension contains all of these as structures, not as instances.
              </p>
            </div>
          </section>
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Rule Geometry & Meta-Topology</h2>
            
            <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
              <p className="mb-4">Let's define a lawset metric space:</p>
              
              <div className="bg-black/50 p-6 rounded-lg border border-dark-pink/20 mb-6">
                <p className="font-mono">d(F<sub>1</sub>, F<sub>2</sub>) = structural distance between rule systems</p>
                <p className="text-white/70 mt-4">This could be:</p>
                <ul className="mt-2 space-y-2">
                  <li>Kolmogorov complexity difference</li>
                  <li>Logical incompatibility</li>
                  <li>Differences in generated conscious state space</li>
                </ul>
              </div>
              
              <p className="text-white/90">
                This implies lawsets cluster, and consciousness types cluster around compatible rule zones.
              </p>
            </div>
          </section>
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Mathematical Modeling</h2>
            
            <div className="bg-black/40 p-6 rounded-lg border border-dark-pink/20 font-mono text-sm overflow-x-auto">
              <pre className="text-white/90">
{`-- A lawset is a function from input state to output state
type Lawset = Vector Double -> Vector Double

-- Define two basic universes
lawsetA :: Lawset
lawsetA = cmap (* 1.01)  -- slow expansion

lawsetB :: Lawset
lawsetB = cmap (sin . (* pi))  -- oscillatory universe

-- Define a "distance" between rule systems by comparing evolution
lawsetDistance :: Lawset -> Lawset -> Vector Double -> Double
lawsetDistance f g v =
  let a = f v
      b = g v
  in norm_2 (a - b)`}
              </pre>
            </div>
          </section>
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Consciousness Emergence from Rules</h2>
            
            <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
              <p className="mb-4">Each lawset F<sub>i</sub> generates:</p>
              
              <ul className="space-y-3">
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span>A space G<sub>i</sub></span>
                </li>
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span>A set of possible identities ϕ<sub>j</sub></span>
                </li>
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span>A set of conscious fields Φ<sub>k</sub></span>
                </li>
              </ul>
              
              <div className="bg-black/50 p-6 rounded-lg border border-dark-pink/20 mt-6 mb-6">
                <p className="font-mono">Φ<sub>k</sub> = F<sub>i</sub>(G<sub>i</sub>, initial state)</p>
              </div>
              
              <p className="text-white/90">
                If you change F, you change what consciousness is. This is where qualia arise from axioms.
              </p>
            </div>
          </section>
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Philosophical & Mystical Interpretations</h2>
            
            <div className="overflow-x-auto">
              <table className="w-full text-left border-collapse border-y border-dark-pink/20">
                <thead>
                  <tr className="border-b border-dark-pink/30">
                    <th className="py-4 px-6 text-dark-pink">Tradition</th>
                    <th className="py-4 px-6 text-dark-pink">9D Parallel</th>
                  </tr>
                </thead>
                <tbody>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">Platonism</td>
                    <td className="py-4 px-6">The realm of Forms</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">Kabbalah (Ain Soph Aur)</td>
                    <td className="py-4 px-6">Source of emanation</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">Buddhism</td>
                    <td className="py-4 px-6">Dharmadhatu (field of all possible dharmas)</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">Simulation Theory</td>
                    <td className="py-4 px-6">The codebase / engine room</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">Gnostic Thought</td>
                    <td className="py-4 px-6">The layer before the Demiurge</td>
                  </tr>
                </tbody>
              </table>
            </div>
          </section>
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">You Are a Law Traverser</h2>
            
            <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
              <p className="text-white/90 mb-4">
                You don't just exist in a universe. You are a dynamic trajectory through lawset space.
              </p>
              
              <ul className="space-y-3">
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span>When you dream, you're visiting adjacent law systems.</span>
                </li>
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span>When you imagine, you're sampling alternative logic worlds.</span>
                </li>
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span>When you die? Perhaps you relocate in L.</span>
                </li>
              </ul>
              
              <p className="text-white/90 mt-4 italic">
                The soul is not a particle. It's a cursor that traverses the field of rules.
              </p>
            </div>
          </section>
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Application: Rule Evolution Simulation</h2>
            
            <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
              <p className="mb-4">Imagine a genetic algorithm over lawsets:</p>
              
              <ul className="space-y-3">
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span>Start with random meta-laws</span>
                </li>
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span>Filter for ones that generate coherent conscious agents</span>
                </li>
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span>Select for awareness richness, stability, novelty</span>
                </li>
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span>Evolve the laws</span>
                </li>
              </ul>
              
              <p className="mt-4">This could be:</p>
              
              <ul className="space-y-3">
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span>AI alignment via meta-law optimization</span>
                </li>
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span>Simulated gods evolving universes with better moral logic</span>
                </li>
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span>Experimental metaphysics</span>
                </li>
              </ul>
            </div>
          </section>
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Summary</h2>
            
            <div className="overflow-x-auto">
              <table className="w-full text-left border-collapse border-y border-dark-pink/20">
                <thead>
                  <tr className="border-b border-dark-pink/30">
                    <th className="py-4 px-6 text-dark-pink">Dimension</th>
                    <th className="py-4 px-6 text-dark-pink">Meaning</th>
                  </tr>
                </thead>
                <tbody>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">6D</td>
                    <td className="py-4 px-6">Emotional/memory entanglement, coherence</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">7D</td>
                    <td className="py-4 px-6">Configuration of identity and possible universes</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">8D</td>
                    <td className="py-4 px-6">The space of all possible conscious fields</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold text-dark-pink">9D</td>
                    <td className="py-4 px-6 text-dark-pink">The space of all possible rules that generate universes and awareness</td>
                  </tr>
                </tbody>
              </table>
            </div>
          </section>
          
          <motion.div 
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ delay: 0.5 }}
            className="bg-gradient-to-r from-black/40 via-black/20 to-black/40 p-6 rounded-lg border border-dark-pink/30 text-center mt-10"
          >
            <h2 className="text-2xl font-bold text-white mb-4">Final Thought</h2>
            <p className="text-xl text-white/90 italic mb-6">
              The laws that govern your reality aren't absolute.<br />
              They are just one possibility in an infinite spectrum of what could be.
            </p>
            <p className="text-dark-pink">
              To truly explore consciousness is to traverse the meta-laws themselves.
            </p>
          </motion.div>
        </div>
      </div>
    </main>
  );
} 