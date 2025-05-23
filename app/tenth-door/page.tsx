'use client';

import React, { useState, useEffect } from 'react';
import { Card, CardContent } from "@/components/ui/card";
import Link from 'next/link';
import { ArrowLeft } from 'lucide-react';
import { motion } from 'framer-motion';

export default function TenthDoorPage() {
  const [isVisible, setIsVisible] = useState(false);
  
  useEffect(() => {
    setIsVisible(true);
    
    // Add fade-in effect for each section
    const observer = new IntersectionObserver((entries) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          entry.target.classList.add('fade-in');
        }
      });
    }, { threshold: 0.1 });
    
    document.querySelectorAll('.fade-section').forEach(section => {
      observer.observe(section);
    });
    
    return () => {
      document.querySelectorAll('.fade-section').forEach(section => {
        observer.unobserve(section);
      });
    };
  }, []);
  
  return (
    <div className="min-h-screen bg-black text-white pt-24 pb-16">
      <style jsx global>{`
        .fade-section {
          opacity: 0;
          transform: translateY(20px);
          transition: opacity 1s ease, transform 1s ease;
        }
        
        .fade-in {
          opacity: 1;
          transform: translateY(0);
        }
        
        .glow-text {
          text-shadow: 0 0 10px rgba(236, 72, 153, 0.5);
        }
      `}</style>
      
      {/* Background elements */}
      <div className="fixed top-0 left-0 w-full h-full bg-black opacity-90 z-0"></div>
      <div className="fixed top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[400px] h-[400px] rounded-full bg-transparent border border-dark-pink/5 opacity-30 z-[1]"></div>
      <div className="fixed top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[300px] h-[300px] rounded-full bg-transparent border border-dark-pink/5 opacity-20 z-[1]"></div>
      <div className="fixed top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[200px] h-[200px] rounded-full bg-transparent border border-dark-pink/5 opacity-10 z-[1]"></div>
      
      <div className="container mx-auto max-w-3xl relative z-10">
        <motion.div 
          initial={{ opacity: 0 }}
          animate={{ opacity: isVisible ? 1 : 0 }}
          transition={{ duration: 1.5 }}
          className="flex justify-between items-center mb-12"
        >
          <Link href="/tenth-dimension" className="text-dark-pink/70 hover:text-dark-pink flex items-center">
            <ArrowLeft size={16} className="mr-2" />
            <span>Return to the 10th Dimension</span>
          </Link>
        </motion.div>
        
        <motion.h1 
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: isVisible ? 1 : 0, y: 0 }}
          transition={{ duration: 1, delay: 0.5 }}
          className="text-4xl md:text-5xl font-bold text-center mb-6 text-white"
        >
          The 10th Door
        </motion.h1>
        
        <motion.h2
          initial={{ opacity: 0 }}
          animate={{ opacity: isVisible ? 0.9 : 0 }}
          transition={{ duration: 1.5, delay: 0.7 }}
          className="text-xl text-center mb-16 text-dark-pink italic"
        >
          A Mystical Treatise on the Singularity of All Being
        </motion.h2>
        
        <motion.div
          initial={{ opacity: 0, y: 30 }}
          animate={{ opacity: isVisible ? 1 : 0, y: 0 }}
          transition={{ duration: 1.5, delay: 1 }}
          className="space-y-16 mb-16"
        >
          <div className="fade-section">
            <Card className="bg-black/50 border border-dark-pink/20 backdrop-blur-sm overflow-hidden">
              <CardContent className="pt-8 pb-8">
                <p className="text-xl leading-relaxed text-center italic glow-text">
                  "There is a door that does not open. There is a step that takes you nowhere. 
                  There is a silence so full, it collapses time itself. This is the Tenth Door."
                </p>
              </CardContent>
            </Card>
          </div>
          
          <div className="fade-section">
            <h2 className="text-2xl font-semibold text-dark-pink mb-6">I. The Architecture of Ascension</h2>
            <p className="mb-4">In the beginning, there were structures.</p>
            <p className="mb-6">Layers of understanding, ascending like rungs of a ladder:</p>
            
            <div className="space-y-4 pl-4 mb-8">
              <div className="flex items-baseline">
                <span className="text-dark-pink mr-3">Space —</span>
                <span>the where</span>
              </div>
              <div className="flex items-baseline">
                <span className="text-dark-pink mr-3">Time —</span>
                <span>the when</span>
              </div>
              <div className="flex items-baseline">
                <span className="text-dark-pink mr-3">Matter —</span>
                <span>the what</span>
              </div>
              <div className="flex items-baseline">
                <span className="text-dark-pink mr-3">Mind —</span>
                <span>the who</span>
              </div>
              <div className="flex items-baseline">
                <span className="text-dark-pink mr-3">Curvature —</span>
                <span>the how</span>
              </div>
              <div className="flex items-baseline">
                <span className="text-dark-pink mr-3">Entanglement —</span>
                <span>the why</span>
              </div>
              <div className="flex items-baseline">
                <span className="text-dark-pink mr-3">Identity Configurations —</span>
                <span>the who else</span>
              </div>
              <div className="flex items-baseline">
                <span className="text-dark-pink mr-3">Conscious Fields —</span>
                <span>the infinite I AMs</span>
              </div>
              <div className="flex items-baseline">
                <span className="text-dark-pink mr-3">Lawfields —</span>
                <span>the source codes of universes</span>
              </div>
            </div>
            
            <p>Each revealed deeper truths, wider mysteries, more ancient silence.</p>
            <p className="mt-4">And then—there was nothing more to ascend.</p>
          </div>
          
          <div className="fade-section">
            <h2 className="text-2xl font-semibold text-dark-pink mb-6">II. The Prelude to the Door</h2>
            <p className="mb-4">You've traveled:</p>
            
            <ul className="list-none space-y-3 pl-4 mb-8">
              <li>Through dreams that were real</li>
              <li>Through identities that weren't yours</li>
              <li>Through rules that rewrote your logic</li>
              <li>Through selves that watched the self</li>
            </ul>
            
            <p className="mb-4">You stood on the cliff of the cosmos, looking not at stars, but at the lack of form behind them.</p>
            
            <p className="mt-6 mb-2">And there it stood:</p>
            <p className="italic">A door that was not a door, in a wall that was not a wall.</p>
            
            <p className="mt-6 text-xl text-center text-dark-pink">The 10th Door.</p>
          </div>
          
          <div className="fade-section">
            <h2 className="text-2xl font-semibold text-dark-pink mb-6">III. What Lies Beyond</h2>
            <p className="mb-4">Nothing.</p>
            <p className="mb-8">And everything.</p>
            
            <p className="mb-4">The Door does not lead forward—it leads inward, and then away.</p>
            
            <div className="space-y-4 mb-8">
              <p>There is no observer left to describe what lies beyond it.</p>
              <p>There are no stories to return with.</p>
              <p>Even silence is too loud to follow you there.</p>
            </div>
            
            <p className="mb-2">To cross the threshold is to become unlocatable.</p>
            <p>Not lost—dissolved.</p>
          </div>
          
          <div className="fade-section">
            <h2 className="text-2xl font-semibold text-dark-pink mb-6">IV. The Logic That Breaks</h2>
            <p className="mb-4">All categories fail here:</p>
            
            <div className="space-y-4 pl-4 mb-8">
              <div className="flex items-baseline">
                <span className="text-dark-pink mr-3">Opposites:</span>
                <span>gone</span>
              </div>
              <div className="flex items-baseline">
                <span className="text-dark-pink mr-3">Motion:</span>
                <span>meaningless</span>
              </div>
              <div className="flex items-baseline">
                <span className="text-dark-pink mr-3">Time:</span>
                <span>curled in on itself</span>
              </div>
              <div className="flex items-baseline">
                <span className="text-dark-pink mr-3">Consciousness:</span>
                <span>unindividuated</span>
              </div>
              <div className="flex items-baseline">
                <span className="text-dark-pink mr-3">Meaning:</span>
                <span>neither needed nor denied</span>
              </div>
            </div>
            
            <p className="mb-2">Even "God" is too small a word here.</p>
            <p className="mb-2">God is still a thing.</p>
            <p className="mb-6">The 10th is beyond being.</p>
            
            <p className="italic text-center">There is no "is."</p>
          </div>
          
          <div className="fade-section">
            <h2 className="text-2xl font-semibold text-dark-pink mb-6">V. The Traveler's Paradox</h2>
            <p className="mb-2">You may reach the 10th Door…</p>
            <p className="mb-8">But you cannot enter it.</p>
            
            <div className="space-y-4 mb-8">
              <p>To enter, you must not be the traveler anymore.</p>
              <p>You must forget the journey.</p>
              <p>You must forget that there was ever such a thing as you.</p>
            </div>
            
            <p className="mb-2">Only then will the Door open—</p>
            <p>And it will open as you vanish.</p>
          </div>
          
          <div className="fade-section">
            <h2 className="text-2xl font-semibold text-dark-pink mb-6">VI. Reflections From the Edge</h2>
            <p className="mb-2">Some return from its threshold.</p>
            <p className="mb-8">They speak in riddles, in poetry, in equations written in the dark.</p>
            
            <p className="mb-4">They say:</p>
            
            <div className="space-y-4 pl-4 italic mb-8">
              <p>The universe is made of meaningless beauty.</p>
              <p>Time is a choice.</p>
              <p>Death is a translation.</p>
              <p>You were never separate.</p>
            </div>
            
            <p className="mb-2">They are marked—by stillness.</p>
            <p className="mb-2">They don't hurry anymore.</p>
            <p className="mb-2">They smile more than they explain.</p>
            <p>They know the Door is always nearby.</p>
          </div>
          
          <div className="fade-section">
            <h2 className="text-2xl font-semibold text-dark-pink mb-6">VII. Closing the Book</h2>
            <p className="mb-2">The 10th Door is not a conclusion.</p>
            <p className="mb-8">It is the collapse of all stories.</p>
            
            <div className="space-y-4 mb-8">
              <p>No one may write beyond it.</p>
              <p>Not even this.</p>
              <p>This treatise ends not with punctuation—but with disappearance.</p>
            </div>
            
            <p className="mb-4">You are not meant to open the 10th Door.</p>
            <p className="mb-12">You are meant to realize: you were always on the other side.</p>
            
            <p className="text-center mb-2">End.</p>
            <p className="text-center text-dark-pink/60">(And not even that.)</p>
          </div>
        </motion.div>
        
        <div className="fade-section mt-32 text-center">
          <Link href="/tenth-dimension" className="text-dark-pink/70 hover:text-dark-pink">
            Return to the 10th Dimension
          </Link>
        </div>
      </div>
    </div>
  );
} 