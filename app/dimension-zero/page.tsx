'use client';

import React, { useState, useEffect, useRef } from 'react';
import { Card, CardContent } from "@/components/ui/card";
import Link from 'next/link';
import { ArrowLeft, ArrowRight } from 'lucide-react';
import { motion } from 'framer-motion';
import { Button } from "@/components/ui/button";

export default function DimensionZeroPage() {
  const [isVisible, setIsVisible] = useState(false);
  const containerRef = useRef<HTMLDivElement>(null);
  
  useEffect(() => {
    setIsVisible(true);
    
    // Create the flickering animation
    const flickers = document.querySelectorAll('.flicker');
    flickers.forEach((flicker, index) => {
      const delay = Math.random() * 5; // Random delay for each flicker
      const duration = 0.1 + Math.random() * 0.3; // Random duration
      const element = flicker as HTMLElement;
      
      // Set initial opacity
      element.style.opacity = '0';
      
      // Create flicker effect
      const flickerAnimation = () => {
        const randomOpacity = Math.random() * 0.7 + 0.3; // Between 0.3 and 1
        element.style.opacity = randomOpacity.toString();
        element.style.transform = `scale(${0.95 + Math.random() * 0.1})`;
        
        // Schedule next flicker
        setTimeout(flickerAnimation, 500 + Math.random() * 2000);
      };
      
      // Start after delay
      setTimeout(flickerAnimation, delay * 1000);
    });
    
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
    <div className="min-h-screen bg-black text-white pt-24 pb-16" ref={containerRef}>
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
        
        .flicker {
          position: fixed;
          background-color: rgba(236, 72, 153, 0.6);
          border-radius: 50%;
          filter: blur(10px);
          z-index: 1;
          transition: opacity 0.2s ease, transform 0.2s ease;
        }
        
        .dust {
          position: absolute;
          background-color: rgba(236, 72, 153, 0.3);
          border-radius: 50%;
          filter: blur(3px);
          z-index: 1;
          opacity: 0.3;
        }
        
        @keyframes float {
          0% { transform: translateY(0px) translateX(0px); }
          50% { transform: translateY(-10px) translateX(5px); }
          100% { transform: translateY(0px) translateX(0px); }
        }
      `}</style>
      
      {/* Background */}
      <div className="fixed top-0 left-0 w-full h-full bg-black opacity-95 z-0"></div>
      
      {/* Flicker particles */}
      <div className="flicker w-1 h-1 top-1/4 left-1/4"></div>
      <div className="flicker w-2 h-2 top-1/3 left-1/2"></div>
      <div className="flicker w-3 h-3 top-2/3 left-1/3"></div>
      <div className="flicker w-1 h-1 top-1/2 left-3/4"></div>
      <div className="flicker w-2 h-2 top-3/4 left-1/4"></div>
      <div className="flicker w-1 h-1 top-1/5 left-2/3"></div>
      <div className="flicker w-4 h-4 top-1/2 left-1/2"></div>
      
      {/* Main content container */}
      <div className="container mx-auto max-w-3xl relative z-10">
        <motion.div 
          initial={{ opacity: 0 }}
          animate={{ opacity: isVisible ? 1 : 0 }}
          transition={{ duration: 1.5 }}
          className="flex justify-between items-center mb-12"
        >
          <Link href="/tenth-door" className="text-dark-pink/70 hover:text-dark-pink flex items-center">
            <ArrowLeft size={16} className="mr-2" />
            <span>Return Through the 10th Door</span>
          </Link>
        </motion.div>
        
        <motion.h1 
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: isVisible ? 1 : 0, y: 0 }}
          transition={{ duration: 1, delay: 0.5 }}
          className="text-4xl md:text-5xl font-bold text-center mb-6 text-white"
        >
          Dimension 0
        </motion.h1>
        
        <motion.h2
          initial={{ opacity: 0 }}
          animate={{ opacity: isVisible ? 0.9 : 0 }}
          transition={{ duration: 1.5, delay: 0.7 }}
          className="text-xl text-center mb-16 text-dark-pink"
        >
          The First Flicker
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
                <p className="text-xl leading-relaxed text-center italic">
                  "In the beginning was not the Word.<br/>
                  In the beginning was the Distinction."
                </p>
              </CardContent>
            </Card>
          </div>
          
          <div className="fade-section">
            <p className="text-center mb-4">Before self.</p>
            <p className="text-center mb-4">Before mind.</p>
            <p className="text-center mb-4">Before silence.</p>
            <p className="text-center mb-4">There was a flicker—the first difference.</p>
          </div>
          
          <div className="fade-section">
            <h2 className="text-2xl font-semibold text-dark-pink mb-6">What Is Dimension 0?</h2>
            <p className="mb-6">
              Dimension 0 is the point before geometry, the root-node of awareness, the pre-ontological seed from which all else unfolds.
            </p>
            
            <div className="space-y-4 mb-8">
              <p>It is not a location in space.</p>
              <p>It is not a state in time.</p>
              <p>It is the first cut—the primal distinction between:</p>
            </div>
            
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4 mb-8">
              <div className="bg-black/40 p-4 rounded-md text-center border border-dark-pink/10">
                <p>Something</p>
              </div>
              <div className="bg-black/40 p-4 rounded-md text-center border border-dark-pink/10">
                <p>not-something</p>
              </div>
              
              <div className="bg-black/40 p-4 rounded-md text-center border border-dark-pink/10">
                <p>Aware</p>
              </div>
              <div className="bg-black/40 p-4 rounded-md text-center border border-dark-pink/10">
                <p>unaware</p>
              </div>
              
              <div className="bg-black/40 p-4 rounded-md text-center border border-dark-pink/10">
                <p>Yes</p>
              </div>
              <div className="bg-black/40 p-4 rounded-md text-center border border-dark-pink/10">
                <p>no</p>
              </div>
            </div>
            
            <p className="mb-4">It is the zero-point function that maps:</p>
            
            <div className="text-center mb-6">
              <p className="text-xl">∅ → Δ</p>
              <p className="text-sm mt-2">Where Δ is the first spark of duality.</p>
            </div>
          </div>
          
          <div className="fade-section">
            <h2 className="text-2xl font-semibold text-dark-pink mb-6">The Flicker of Self</h2>
            <p className="mb-4">At Dimension 0, there is not yet a self.</p>
            <p className="mb-6">But the possibility of self has emerged.</p>
            
            <div className="text-center mb-8">
              <p className="text-xl italic">"I might be."</p>
            </div>
            
            <p className="mb-4">This is the ur-thought behind all dreams, all gods, all dimensions.</p>
            <p className="mb-2">Not "I am."</p>
            <p className="mb-4">Just: "Is?"</p>
          </div>
          
          <div className="fade-section">
            <h2 className="text-2xl font-semibold text-dark-pink mb-6">The Birth of Form</h2>
            <p className="mb-6">From 0 emerges 1.</p>
            <p className="mb-6">From the undifferentiated void comes the axis of relation.</p>
            
            <div className="text-center mb-8">
              <p className="text-xl">0 → 1 → ∞</p>
            </div>
            
            <div className="space-y-4 mb-8">
              <p>Dimension 1 will define a line.</p>
              <p>Dimension 2 will draw duality.</p>
              <p>Dimension 3 will open space.</p>
              <p>And so on…</p>
            </div>
            
            <p className="mb-4">But all of it was nested in the original flicker.</p>
            <p className="mb-2">Like a tree in a seed.</p>
            <p className="mb-4">Like fire in a match.</p>
          </div>
          
          <div className="fade-section">
            <h2 className="text-2xl font-semibold text-dark-pink mb-6">The Root of All Return</h2>
            <p className="mb-4">You don't reach Dimension 0 by descending.</p>
            <p className="mb-4">You arrive by remembering.</p>
            <p className="mb-2">Remembering the first question before thought.</p>
            <p className="mb-2">The first curve before line.</p>
            <p className="mb-4">The first feeling before name.</p>
          </div>
          
          <div className="fade-section">
            <h2 className="text-2xl font-semibold text-dark-pink mb-6">Final Meditation</h2>
            <div className="bg-black/30 border border-dark-pink/20 p-6 rounded-md">
              <p className="italic text-center">
                "Before the universe, I existed not as a self, but as a question.<br/>
                And that question became the thread upon which existence was woven."
              </p>
            </div>
            
            <div className="mt-12 space-y-4">
              <p className="text-center">And so…</p>
              <p className="text-center">You return to Dimension 0</p>
              <p className="text-center">Not as a traveler</p>
              <p className="text-center">Not as a god</p>
              <p className="text-center">But as a flicker—ready again to become.</p>
            </div>
          </div>
          
          <div className="fade-section">
            <h2 className="text-2xl font-semibold text-dark-pink mb-8 text-center">Choose:</h2>
            
            <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-12">
              <Link href="/dream-traversal">
                <div className="bg-black/30 border border-dark-pink/20 hover:border-dark-pink p-6 rounded-md text-center h-full flex flex-col justify-between transition-colors group">
                  <p className="group-hover:text-dark-pink">To ascend again, weaving new dimensions?</p>
                  <div className="mt-4">
                    <Button variant="outline" className="border-dark-pink/50 text-dark-pink/80 hover:bg-dark-pink/10 hover:text-dark-pink">
                      Ascend
                    </Button>
                  </div>
                </div>
              </Link>
              
              <div className="bg-black/30 border border-dark-pink/20 hover:border-dark-pink p-6 rounded-md text-center h-full flex flex-col justify-between transition-colors group">
                <p className="group-hover:text-dark-pink">To stay, in sacred pre-being?</p>
                <div className="mt-4">
                  <Button variant="outline" className="border-dark-pink/50 text-dark-pink/80 hover:bg-dark-pink/10 hover:text-dark-pink" onClick={() => window.scrollTo({ top: 0, behavior: 'smooth' })}>
                    Stay
                  </Button>
                </div>
              </div>
              
              <Link href="/">
                <div className="bg-black/30 border border-dark-pink/20 hover:border-dark-pink p-6 rounded-md text-center h-full flex flex-col justify-between transition-colors group">
                  <p className="group-hover:text-dark-pink">Or to scatter, seeding flickers across worlds?</p>
                  <div className="mt-4">
                    <Button variant="outline" className="border-dark-pink/50 text-dark-pink/80 hover:bg-dark-pink/10 hover:text-dark-pink">
                      Scatter
                    </Button>
                  </div>
                </div>
              </Link>
            </div>
            
            <p className="text-center">Your journey is yours.</p>
            <p className="text-center mt-4">
              And it begins, always,<br/>
              with the first flicker.
            </p>
          </div>
        </motion.div>
        
        <motion.div 
          initial={{ opacity: 0 }}
          animate={{ opacity: isVisible ? 1 : 0 }}
          transition={{ duration: 1.5, delay: 7 }}
          className="mt-32 text-center"
        >
          <div className="flex flex-col items-center gap-4">
            <Link href="/tenth-dimension" className="text-dark-pink/70 hover:text-dark-pink">
              Return to the 10th Dimension
            </Link>
            <Link href="/tenth-door" className="text-dark-pink/70 hover:text-dark-pink">
              Return to the 10th Door
            </Link>
          </div>
        </motion.div>
      </div>
    </div>
  );
} 