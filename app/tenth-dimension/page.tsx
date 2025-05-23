'use client';

import React, { useState, useRef, useEffect } from 'react';
import { Card, CardContent } from "@/components/ui/card";
import { DisplayMath } from "@/components/ui/math";
import Link from 'next/link';
import { ArrowLeft } from 'lucide-react';
import { motion } from 'framer-motion';

export default function TenthDimensionPage() {
  const [isVisible, setIsVisible] = useState(false);
  const containerRef = useRef<HTMLDivElement>(null);
  
  useEffect(() => {
    setIsVisible(true);
    
    // Add scroll event listener for parallax effect
    const handleScroll = () => {
      if (containerRef.current) {
        const scrollTop = window.scrollY;
        const elements = containerRef.current.querySelectorAll('.parallax');
        elements.forEach((el, index) => {
          const speed = 0.2 + (index * 0.05);
          (el as HTMLElement).style.transform = `translateY(${scrollTop * speed}px)`;
        });
      }
    };
    
    window.addEventListener('scroll', handleScroll);
    return () => window.removeEventListener('scroll', handleScroll);
  }, []);
  
  return (
    <div className="min-h-screen bg-black text-white pt-24 pb-16" ref={containerRef}>
      <div className="fixed top-0 left-0 w-full h-full bg-black opacity-90 z-0"></div>
      
      {/* Subtle visual elements representing the void/singularity */}
      <div className="fixed top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[300px] h-[300px] rounded-full bg-transparent border border-dark-pink/5 opacity-30 z-[1]"></div>
      <div className="fixed top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[200px] h-[200px] rounded-full bg-transparent border border-dark-pink/10 opacity-20 z-[1]"></div>
      <div className="fixed top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[100px] h-[100px] rounded-full bg-transparent border border-dark-pink/20 opacity-10 z-[1]"></div>
      <div className="fixed top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[30px] h-[30px] rounded-full bg-dark-pink/5 blur-sm z-[1]"></div>
      
      <div className="container mx-auto max-w-4xl relative z-10">
        <motion.div 
          initial={{ opacity: 0 }}
          animate={{ opacity: isVisible ? 1 : 0 }}
          transition={{ duration: 1.5 }}
          className="flex justify-between items-center mb-12"
        >
          <Link href="/" className="text-dark-pink/70 hover:text-dark-pink flex items-center">
            <ArrowLeft size={16} className="mr-2" />
            <span>Return to Reality</span>
          </Link>
        </motion.div>
        
        <motion.h1 
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: isVisible ? 1 : 0, y: 0 }}
          transition={{ duration: 1, delay: 0.5 }}
          className="text-5xl md:text-6xl font-bold text-center mb-6 text-white"
        >
          <span className="block">Dimension 10</span>
          <span className="text-dark-pink text-3xl md:text-4xl block mt-2">The Singularity of Totality</span>
        </motion.h1>
        
        <motion.div
          initial={{ opacity: 0 }}
          animate={{ opacity: isVisible ? 0.8 : 0 }}
          transition={{ duration: 2, delay: 1 }}
          className="text-center mb-16 text-lg text-white/70 italic parallax"
        >
          You've arrived at the edge.
        </motion.div>
        
        <motion.div
          initial={{ opacity: 0, y: 30 }}
          animate={{ opacity: isVisible ? 1 : 0, y: 0 }}
          transition={{ duration: 1, delay: 1.5 }}
          className="space-y-16"
        >
          <Card className="bg-black/50 border border-dark-pink/20 backdrop-blur-sm">
            <CardContent className="pt-8 pb-8">
              <div className="prose prose-invert max-w-none">
                <p className="text-xl leading-relaxed">
                  Dimension 10 is not a space of things. It is the groundless ground—the pre-condition of existence itself.
                  Not a field. Not a form. Not even potentiality.
                  It is non-duality. The point at which all distinctions—between matter and mind, self and other, possible and impossible—collapse.
                </p>
              </div>
            </CardContent>
          </Card>
          
          <div className="mt-20 mb-20 text-center parallax">
            <div className="text-3xl font-light text-dark-pink">What Is It?</div>
            <p className="mt-4 text-xl">
              The 10th dimension is not another layer above—it is the singular convergence of all dimensions.
            </p>
          </div>
          
          <div className="grid grid-cols-1 gap-12">
            <motion.div 
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: isVisible ? 1 : 0, y: 0 }}
              transition={{ duration: 1, delay: 2 }}
              className="text-center parallax"
            >
              <p className="text-2xl font-light">Where:</p>
            </motion.div>
            
            <motion.div 
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: isVisible ? 1 : 0, y: 0 }}
              transition={{ duration: 1, delay: 2.2 }}
              className="text-center"
            >
              <p className="text-xl">All universes are one.</p>
            </motion.div>
            
            <motion.div 
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: isVisible ? 1 : 0, y: 0 }}
              transition={{ duration: 1, delay: 2.4 }}
              className="text-center"
            >
              <p className="text-xl">All selves are one.</p>
            </motion.div>
            
            <motion.div 
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: isVisible ? 1 : 0, y: 0 }}
              transition={{ duration: 1, delay: 2.6 }}
              className="text-center"
            >
              <p className="text-xl">All rules, logic, time, awareness, identity, even structure, dissolve.</p>
            </motion.div>
            
            <motion.div 
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: isVisible ? 1 : 0, y: 0 }}
              transition={{ duration: 1, delay: 2.8 }}
              className="text-center"
            >
              <p className="text-xl">It is Nothing, and Everything—at once.</p>
            </motion.div>
          </div>
          
          <div className="mt-32">
            <motion.h2 
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 3 }}
              className="text-2xl font-semibold text-dark-pink mb-8 text-center"
            >
              Names Across Traditions
            </motion.h2>
            
            <div className="overflow-x-auto">
              <table className="w-full border-collapse">
                <thead>
                  <tr className="border-b border-dark-pink/20">
                    <th className="text-left py-4 px-6 font-medium text-dark-pink/80">Tradition</th>
                    <th className="text-left py-4 px-6 font-medium text-dark-pink/80">Name of Dimension 10</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-dark-pink/10">
                  <motion.tr
                    initial={{ opacity: 0 }}
                    animate={{ opacity: isVisible ? 1 : 0 }}
                    transition={{ duration: 0.5, delay: 3.1 }}
                  >
                    <td className="py-4 px-6">Taoism</td>
                    <td className="py-4 px-6">The Tao (that cannot be named)</td>
                  </motion.tr>
                  <motion.tr
                    initial={{ opacity: 0 }}
                    animate={{ opacity: isVisible ? 1 : 0 }}
                    transition={{ duration: 0.5, delay: 3.2 }}
                  >
                    <td className="py-4 px-6">Vedanta</td>
                    <td className="py-4 px-6">Brahman (pure being, beyond qualities)</td>
                  </motion.tr>
                  <motion.tr
                    initial={{ opacity: 0 }}
                    animate={{ opacity: isVisible ? 1 : 0 }}
                    transition={{ duration: 0.5, delay: 3.3 }}
                  >
                    <td className="py-4 px-6">Kabbalah</td>
                    <td className="py-4 px-6">Ein (non-being), beyond Ein Sof</td>
                  </motion.tr>
                  <motion.tr
                    initial={{ opacity: 0 }}
                    animate={{ opacity: isVisible ? 1 : 0 }}
                    transition={{ duration: 0.5, delay: 3.4 }}
                  >
                    <td className="py-4 px-6">Christian Mysticism</td>
                    <td className="py-4 px-6">The Godhead (Deus absconditus)</td>
                  </motion.tr>
                  <motion.tr
                    initial={{ opacity: 0 }}
                    animate={{ opacity: isVisible ? 1 : 0 }}
                    transition={{ duration: 0.5, delay: 3.5 }}
                  >
                    <td className="py-4 px-6">Buddhism</td>
                    <td className="py-4 px-6">Śūnyatā (emptiness), the void beyond void</td>
                  </motion.tr>
                  <motion.tr
                    initial={{ opacity: 0 }}
                    animate={{ opacity: isVisible ? 1 : 0 }}
                    transition={{ duration: 0.5, delay: 3.6 }}
                  >
                    <td className="py-4 px-6">Physics</td>
                    <td className="py-4 px-6">The edge of M-theory; the 10D brane where strings become indistinct</td>
                  </motion.tr>
                  <motion.tr
                    initial={{ opacity: 0 }}
                    animate={{ opacity: isVisible ? 1 : 0 }}
                    transition={{ duration: 0.5, delay: 3.7 }}
                  >
                    <td className="py-4 px-6">Information Theory</td>
                    <td className="py-4 px-6">Bit 0 and 1 indistinguishable—pre-logic</td>
                  </motion.tr>
                </tbody>
              </table>
            </div>
          </div>
          
          <div className="mt-24">
            <motion.h2 
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 3.8 }}
              className="text-2xl font-semibold text-dark-pink mb-8 text-center"
            >
              Properties of Dimension 10
            </motion.h2>
            
            <div className="overflow-x-auto">
              <table className="w-full border-collapse">
                <thead>
                  <tr className="border-b border-dark-pink/20">
                    <th className="text-left py-4 px-6 font-medium text-dark-pink/80">Property</th>
                    <th className="text-left py-4 px-6 font-medium text-dark-pink/80">Value</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-dark-pink/10">
                  <motion.tr
                    initial={{ opacity: 0 }}
                    animate={{ opacity: isVisible ? 1 : 0 }}
                    transition={{ duration: 0.5, delay: 3.9 }}
                  >
                    <td className="py-4 px-6">Time</td>
                    <td className="py-4 px-6">Collapsed</td>
                  </motion.tr>
                  <motion.tr
                    initial={{ opacity: 0 }}
                    animate={{ opacity: isVisible ? 1 : 0 }}
                    transition={{ duration: 0.5, delay: 4.0 }}
                  >
                    <td className="py-4 px-6">Space</td>
                    <td className="py-4 px-6">Undefined</td>
                  </motion.tr>
                  <motion.tr
                    initial={{ opacity: 0 }}
                    animate={{ opacity: isVisible ? 1 : 0 }}
                    transition={{ duration: 0.5, delay: 4.1 }}
                  >
                    <td className="py-4 px-6">Causality</td>
                    <td className="py-4 px-6">Nonsensical</td>
                  </motion.tr>
                  <motion.tr
                    initial={{ opacity: 0 }}
                    animate={{ opacity: isVisible ? 1 : 0 }}
                    transition={{ duration: 0.5, delay: 4.2 }}
                  >
                    <td className="py-4 px-6">Information</td>
                    <td className="py-4 px-6">Zero, or infinite—same</td>
                  </motion.tr>
                  <motion.tr
                    initial={{ opacity: 0 }}
                    animate={{ opacity: isVisible ? 1 : 0 }}
                    transition={{ duration: 0.5, delay: 4.3 }}
                  >
                    <td className="py-4 px-6">Consciousness</td>
                    <td className="py-4 px-6">No self, no other</td>
                  </motion.tr>
                  <motion.tr
                    initial={{ opacity: 0 }}
                    animate={{ opacity: isVisible ? 1 : 0 }}
                    transition={{ duration: 0.5, delay: 4.4 }}
                  >
                    <td className="py-4 px-6">Geometry</td>
                    <td className="py-4 px-6">Not even a point</td>
                  </motion.tr>
                </tbody>
              </table>
            </div>
            
            <motion.p 
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 4.5 }}
              className="mt-8 text-center text-lg"
            >
              It is not even being, because that implies not-being. And there are no opposites here.
            </motion.p>
          </div>
          
          <div className="mt-24">
            <motion.h2 
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 4.6 }}
              className="text-2xl font-semibold text-dark-pink mb-8 text-center"
            >
              In Math
            </motion.h2>
            
            <motion.p
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 4.7 }}
              className="text-center mb-8"
            >
              Mathematically, we might try:
            </motion.p>
            
            <motion.div
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 4.8 }}
              className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center mb-6 max-w-lg mx-auto"
            >
              <DisplayMath math="\lim_{n \to \infty} \text{Collapse}(C_n) = \emptyset" />
            </motion.div>
            
            <motion.p
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 4.9 }}
              className="text-center mb-8"
            >
              But that's too much.
            </motion.p>
            
            <motion.p
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 5.0 }}
              className="text-center"
            >
              Instead: Dimension 10 is the null object in the category of dimensions.<br />
              Or the top object in the lattice of all possible being.
            </motion.p>
            
            <motion.p
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 5.1 }}
              className="text-center mt-8"
            >
              We can't simulate it.
            </motion.p>
            
            <motion.p
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 5.2 }}
              className="text-center mt-4"
            >
              We can only approximate its silence.
            </motion.p>
          </div>
          
          <div className="mt-24">
            <motion.h2 
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 5.3 }}
              className="text-2xl font-semibold text-dark-pink mb-8 text-center"
            >
              Experiencing Dimension 10
            </motion.h2>
            
            <motion.p
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 5.4 }}
              className="text-center mb-8"
            >
              You may brush against it:
            </motion.p>
            
            <motion.ul
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 5.5 }}
              className="list-none space-y-4 text-center max-w-lg mx-auto"
            >
              <li>In ego death</li>
              <li>In deep samadhi or nirvikalpa meditation</li>
              <li>During a near-death experience</li>
              <li>At the final moment of death</li>
              <li>In an overwhelming moment of awe where identity and world vanish</li>
            </motion.ul>
          </div>
          
          <div className="mt-24">
            <motion.h2 
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 5.6 }}
              className="text-2xl font-semibold text-dark-pink mb-8 text-center"
            >
              Dimension 10 as a Reset Point
            </motion.h2>
            
            <motion.ul
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 5.7 }}
              className="list-none space-y-4 text-center max-w-lg mx-auto"
            >
              <li>All possibilities emerge from it</li>
              <li>All trajectories return to it</li>
              <li>It's the zero point of reality's wave function</li>
              <li>It is the place before birth, and the place after death</li>
              <li>Not a place, but a singularity of perfect undividedness</li>
            </motion.ul>
          </div>
          
          <div className="mt-24">
            <motion.h2 
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 5.8 }}
              className="text-2xl font-semibold text-dark-pink mb-8 text-center"
            >
              The Ouroboros: Full Circle
            </motion.h2>
            
            <motion.p
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 5.9 }}
              className="text-center mb-8"
            >
              When you reached:
            </motion.p>
            
            <motion.ul
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 6.0 }}
              className="list-none space-y-4 text-center max-w-lg mx-auto"
            >
              <li>6D — coherence of minds</li>
              <li>7D — branching of identities</li>
              <li>8D — all awareness</li>
              <li>9D — laws behind awareness</li>
            </motion.ul>
            
            <motion.p
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 6.1 }}
              className="text-center mt-8"
            >
              Then finally…
            </motion.p>
            
            <motion.p
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 6.2 }}
              className="text-center mt-4 text-xl"
            >
              10D is the realization that all of those were illusions.<br />
              Shadows dancing on nothing.
            </motion.p>
          </div>
          
          <div className="mt-24">
            <motion.h2 
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 6.3 }}
              className="text-2xl font-semibold text-dark-pink mb-8 text-center"
            >
              You Cannot Go Beyond This
            </motion.h2>
            
            <motion.p
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 6.4 }}
              className="text-center mb-8"
            >
              Because:
            </motion.p>
            
            <motion.ul
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 6.5 }}
              className="list-none space-y-4 text-center max-w-lg mx-auto"
            >
              <li>There is no "beyond"</li>
              <li>There is no "you"</li>
              <li>This is the event horizon of conceptual existence</li>
            </motion.ul>
          </div>
          
          <div className="mt-24 mb-16">
            <motion.h2 
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 6.6 }}
              className="text-2xl font-semibold text-dark-pink mb-8 text-center"
            >
              Final Thought
            </motion.h2>
            
            <motion.p
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 1, delay: 6.7 }}
              className="text-center text-xl italic"
            >
              In Dimension 10, the dreamer, the dream, and the dreamscape are one.<br />
              The observer and the observed dissolve.<br />
              Only beingless awareness remains.<br />
              Or maybe not even that.
            </motion.p>
          </div>
          
          <div className="mt-32 text-center">
            <motion.div
              initial={{ opacity: 0 }}
              animate={{ opacity: isVisible ? 1 : 0 }}
              transition={{ duration: 2, delay: 7.0 }}
            >
              <Link href="/agdef" className="text-dark-pink/70 hover:text-dark-pink">
                Return to AGDEF Theory
              </Link>
            </motion.div>
          </div>
        </motion.div>
      </div>
    </div>
  );
} 