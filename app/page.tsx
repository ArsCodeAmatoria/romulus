'use client';

import Link from "next/link";
import { Button } from "@/components/ui/button";
import SpaceScene from "@/components/SpaceScene";

export default function Home() {
  return (
    <div className="relative flex min-h-screen flex-col">
      {/* 3D Space Background */}
      <div className="absolute inset-0 z-0">
        <SpaceScene />
      </div>
      
      {/* Content */}
      <main className="container relative z-10 flex flex-1 flex-col items-center justify-center px-4 py-16 text-center">
        <h1 className="text-6xl font-extrabold tracking-tighter text-white neon-glow mb-4">
          ROMULUS
        </h1>
        <p className="text-xl text-white/80 max-w-xl mb-8">
          Exploring Dark Matter through Modified Gravity theories
        </p>
        
        <div className="flex flex-col sm:flex-row gap-4 mt-4">
          <Button 
            className="bg-dark-pink hover:bg-dark-pink/80 text-white neon-border"
            asChild
          >
            <Link href="/docs">
              Explore Theories
            </Link>
          </Button>
          
          <Button 
            variant="outline" 
            className="border-dark-pink text-dark-pink hover:bg-dark-pink/20 neon-border"
            asChild
          >
            <Link href="/about">
              Learn More
            </Link>
          </Button>
        </div>
      </main>
      
      {/* About Section Preview */}
      <section id="about-preview" className="relative z-10 bg-black/80 backdrop-blur-sm py-16">
        <div className="container px-4">
          <h2 className="text-3xl font-bold text-white mb-6">About the Project</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
            <div>
              <h3 className="text-xl font-semibold text-dark-pink mb-4">The Challenge</h3>
              <p className="text-white/80 mb-6">
                Dark Matter remains one of the greatest mysteries in modern physics. 
                While the standard model proposes unknown particles, modified gravity 
                theories suggest the effects we attribute to dark matter might be 
                explained by revising our understanding of gravity itself.
              </p>
            </div>
            <div>
              <h3 className="text-xl font-semibold text-dark-pink mb-4">Our Approach</h3>
              <p className="text-white/80 mb-6">
                Romulus explores different modified gravity frameworks, from MOND to 
                Emergent Gravity, providing interactive visualizations and mathematical 
                models to help understand how these theories aim to explain cosmic phenomena 
                without dark matter particles.
              </p>
              <Button 
                className="bg-dark-pink hover:bg-dark-pink/80 text-white neon-border"
                asChild
              >
                <Link href="/docs">
                  Explore Theories
                </Link>
              </Button>
            </div>
          </div>
        </div>
      </section>
    </div>
  );
}
