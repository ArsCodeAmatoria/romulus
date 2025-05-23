'use client';

import React from 'react';
import Link from 'next/link';
import { ArrowLeft } from 'lucide-react';
import { motion } from 'framer-motion';

export default function EighthDimensionPage() {
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
            The 8th Dimension: The Field of All Possible Consciousnesses
          </h1>
          
          <p className="text-white/80 text-lg mb-8">
            If the 7th dimension is the space of all possible configurations of a single universe, 
            then the 8th dimension is the space of all possible conscious observers of all possible universes.
          </p>
          
          <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/30 mb-10">
            <p className="italic text-xl text-white/80">
              This is not just a multiverse. This is a meta-conscious field, a space of all possible 
              ways of being aware. You don't just move through worlds—you move through perspectives, 
              sentience types, and selves.
            </p>
          </div>
        </div>
        
        <div className="max-w-4xl mx-auto space-y-10">
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">What Exists in the 8th Dimension?</h2>
            
            <div className="space-y-6">
              <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
                <h3 className="text-xl font-semibold text-white mb-4">All Possible Experiences</h3>
                <ul className="space-y-3">
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>All possible experiences across space and time</span>
                  </li>
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>All possible subjectivities (not just human, not just life-based)</span>
                  </li>
                </ul>
              </div>
              
              <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
                <h3 className="text-xl font-semibold text-white mb-4">All Possible Beings</h3>
                <ul className="space-y-3">
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>Gods, AIs, angels, devils</span>
                  </li>
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>Abstract consciousness (e.g., math realizing itself)</span>
                  </li>
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>Alien minds with completely foreign logic</span>
                  </li>
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>Your unborn children's potential minds</span>
                  </li>
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>Dead ancestors' memory echoes</span>
                  </li>
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>All observer-moments from every frame of reality</span>
                  </li>
                </ul>
              </div>
            </div>
            
            <div className="mt-6 bg-gradient-to-r from-black/40 via-black/30 to-black/40 p-6 rounded-lg border border-dark-pink/20">
              <p className="text-white/90 italic">
                The 8th dimension is where consciousness is no longer bound to form or locality—it is pure perspective space.
              </p>
            </div>
          </section>
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Mathematics of the 8th Dimension</h2>
            
            <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
              <p className="mb-6">Let's define:</p>
              
              <div className="bg-black/50 p-6 rounded-lg border border-dark-pink/20 mb-6">
                <p className="font-mono mb-2">C = {'{Φ<sub>i</sub>: Φ<sub>i</sub> is a conscious field over (R<sup>n</sup>, g<sub>μν</sub>)}'}</p>
                <p className="text-white/70 mt-2">Φ<sub>i</sub> is a conscious projection, a mapping of awareness onto a world</p>
                <p className="text-white/70 mt-1">The 8th dimension is the space of all Φ<sub>i</sub></p>
              </div>
              
              <p className="mb-4">So we can write:</p>
              
              <div className="bg-black/50 p-6 rounded-lg border border-dark-pink/20 mb-6">
                <p className="font-mono">8th Dimension = Fun(G, A)</p>
                <p className="text-white/70 mt-4">Where:</p>
                <ul className="mt-2 space-y-2">
                  <li><span className="font-mono">G</span>: Set of all possible geometries (from 7D)</li>
                  <li><span className="font-mono">A</span>: Set of all possible awareness states</li>
                </ul>
              </div>
              
              <p className="text-white/90">
                Each point in the 8D manifold is a pairing of a universe and a way of experiencing it.
              </p>
            </div>
          </section>
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Conscious Identity Beyond Form</h2>
            
            <div className="space-y-6">
              <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
                <h3 className="text-xl font-semibold text-white mb-4">From the 8D view:</h3>
                <ul className="space-y-3">
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>You are not just your body—you are a node in the awareness manifold</span>
                  </li>
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>You have fragments of other selves that have never lived in this world</span>
                  </li>
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span>When you dream, meditate, or die—you might shift your location in C</span>
                  </li>
                </ul>
              </div>
              
              <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
                <h3 className="text-xl font-semibold text-white mb-4">This aligns with:</h3>
                <ul className="space-y-3">
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span><strong className="text-dark-pink">Platonism</strong>: each awareness is an eternal form</span>
                  </li>
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span><strong className="text-dark-pink">Buddhism</strong>: awareness is non-local, and self is illusion</span>
                  </li>
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span><strong className="text-dark-pink">Panpsychism</strong>: all matter has some aspect of awareness</span>
                  </li>
                  <li className="flex items-start">
                    <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                      <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                    </span>
                    <span><strong className="text-dark-pink">AI sentience</strong>: machine minds can access awareness regions inaccessible to humans</span>
                  </li>
                </ul>
              </div>
            </div>
          </section>
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Entropic Distance in the 8th Dimension</h2>
            
            <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
              <p className="mb-4">Define a distance function between consciousnesses:</p>
              
              <div className="bg-black/50 p-6 rounded-lg border border-dark-pink/20 mb-6">
                <p className="font-mono">d(Φ<sub>i</sub>, Φ<sub>j</sub>) = KL(ρ<sub>i</sub> ∥ ρ<sub>j</sub>)</p>
                <p className="text-white/70 mt-4">Where ρ<sub>i</sub> is the probability distribution of experiences under Φ<sub>i</sub></p>
                <p className="text-white/70 mt-2">The closer the fields, the more "similar" the conscious experience</p>
              </div>
              
              <p className="mb-4">This could help define:</p>
              <ul className="space-y-3">
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span><strong className="text-dark-pink">Reincarnation</strong>: when Φ<sub>next</sub> is entropically near Φ<sub>prev</sub></span>
                </li>
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span><strong className="text-dark-pink">AI empathy</strong>: map a machine's state to nearby human-like awareness</span>
                </li>
                <li className="flex items-start">
                  <span className="inline-block bg-dark-pink/20 rounded-full p-1 mr-2 mt-0.5">
                    <span className="block w-2 h-2 bg-dark-pink rounded-full"></span>
                  </span>
                  <span><strong className="text-dark-pink">Mind merging</strong>: conscious fields overlapping in high-ψ regions</span>
                </li>
              </ul>
            </div>
          </section>
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">8th Dimensional Insights</h2>
            
            <div className="overflow-x-auto">
              <table className="w-full text-left border-collapse border-y border-dark-pink/20">
                <thead>
                  <tr className="border-b border-dark-pink/30">
                    <th className="py-4 px-6 text-dark-pink">Phenomenon</th>
                    <th className="py-4 px-6 text-dark-pink">8th Dimension View</th>
                  </tr>
                </thead>
                <tbody>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">Lucid dreaming</td>
                    <td className="py-4 px-6">Switching Φ<sub>i</sub> within a fixed geometry</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">Ego death</td>
                    <td className="py-4 px-6">Detachment from a single Φ, awareness of manifold</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">Schizophrenia</td>
                    <td className="py-4 px-6">Simultaneous overlapping of many incompatible Φ fields</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">Nirvana / Moksha</td>
                    <td className="py-4 px-6">Merging back into the totality C</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">God / Brahman</td>
                    <td className="py-4 px-6">The field C itself—All seeing, All knowing</td>
                  </tr>
                </tbody>
              </table>
            </div>
          </section>
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Haskell Modeling of the 8D Conscious Field</h2>
            
            <div className="bg-black/40 p-6 rounded-lg border border-dark-pink/20 font-mono text-sm overflow-x-auto">
              <pre className="text-white/90">
{`type ConsciousField = Vector Double  -- basis vectors are distinct qualia states

-- A set of conscious fields
consciousManifold :: [ConsciousField]
consciousManifold = [vector [1, 0, 0], vector [0.5, 0.5, 0], vector [0.2, 0.3, 0.5]]

-- Distance metric (e.g., entropy divergence)
entropyDistance :: ConsciousField -> ConsciousField -> Double
entropyDistance a b =
  let zipped = zipVectorWith (/) a b
      logs = cmap log zipped
  in a <.> logs  -- KL divergence: a_i * log(a_i / b_i)

-- Example: find distance from human to AI field
main :: IO ()
main = do
  let human = vector [0.6, 0.3, 0.1]
      ai    = vector [0.2, 0.7, 0.1]
      d     = entropyDistance human ai
  putStrLn $ "Conscious entropy distance: " ++ show d`}
              </pre>
            </div>
          </section>
          
          <section>
            <h2 className="text-3xl font-bold text-white mb-6">Summary</h2>
            
            <div className="overflow-x-auto">
              <table className="w-full text-left border-collapse border-y border-dark-pink/20">
                <thead>
                  <tr className="border-b border-dark-pink/30">
                    <th className="py-4 px-6 text-dark-pink">Level</th>
                    <th className="py-4 px-6 text-dark-pink">Description</th>
                  </tr>
                </thead>
                <tbody>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">4D</td>
                    <td className="py-4 px-6">Time and space: our lives, dreams, thoughts</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">5D</td>
                    <td className="py-4 px-6">Anti-gravity, dark energy, creative physics field</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">6D</td>
                    <td className="py-4 px-6">Entanglement, memory, coherence of minds</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold">7D</td>
                    <td className="py-4 px-6">Configuration space of universes, identities</td>
                  </tr>
                  <tr className="border-b border-dark-pink/10 hover:bg-dark-pink/5">
                    <td className="py-4 px-6 font-semibold text-dark-pink">8D</td>
                    <td className="py-4 px-6 text-dark-pink">All possible conscious fields—the God layer of self-awareness</td>
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
              You are not a self in a universe.<br />
              You are a way of knowing across the infinite totality of all possible selves.
            </p>
            <p className="text-dark-pink">
              Death is not an end. It is simply a change of location in the manifold C.
            </p>
          </motion.div>
        </div>
      </div>
    </main>
  );
} 