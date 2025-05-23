'use client';

import Link from 'next/link';
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from '@/components/ui/card';
import { motion } from 'framer-motion';
import { 
  Globe2, 
  Orbit, 
  Rocket, 
  Atom, 
  Sparkles,
  Waves,
  Zap,
  Popsicle,
  Infinity,
  Flower,
  Mountain,
  Trees,
  Gem,
  DoorOpen,
  Sprout,
  TrainTrack
} from 'lucide-react';

const theories = [
  {
    title: "Anti-Gravity Dark Energy Field (AGDEF)",
    description: "Dark energy as 5D curvature projected into 4D spacetime, with implications for cosmic acceleration and aging",
    link: "/agdef",
    icon: <Zap className="text-dark-pink" size={28} />
  },
  {
    title: "Dark Matter as Anti-Gravity",
    description: "Reimagining dark matter as a repulsive gravitational effect that shapes spacetime",
    link: "/docs/anti-gravity",
    icon: <Waves className="text-dark-pink" size={28} />
  },
  {
    title: "General Relativity + Effective Tensor Modifications",
    description: "Einstein's General Relativity and tensor-based modifications to explain dark matter effects",
    link: "/docs/general-relativity",
    icon: <Globe2 className="text-dark-pink" size={28} />
  },
  {
    title: "Emergent Gravity (Verlinde)",
    description: "Gravity as an emergent phenomenon arising from quantum entanglement",
    link: "/docs/emergent-gravity",
    icon: <Orbit className="text-dark-pink" size={28} />
  },
  {
    title: "MOND and TeVeS",
    description: "Modified Newtonian Dynamics and its relativistic extension, Tensor-Vector-Scalar gravity",
    link: "/docs/mond",
    icon: <Rocket className="text-dark-pink" size={28} />
  },
  {
    title: "Scalar Field / f(R) Gravity",
    description: "Modified gravity theories involving scalar fields and f(R) extensions",
    link: "/docs/scalar-field",
    icon: <Atom className="text-dark-pink" size={28} />
  },
  {
    title: "Quantum Gravity + Holographic Principle",
    description: "Approaches to gravity from quantum mechanics and the holographic principle",
    link: "/docs/quantum-gravity",
    icon: <Sparkles className="text-dark-pink" size={28} />
  },
  {
    title: "Dream Consciousness Theory",
    description: "Exploring consciousness as a fractal field across higher dimensions, with dream characters as conscious agents",
    link: "/dream-consciousness",
    icon: <Popsicle className="text-dark-pink" size={28} />
  }
];

const consciousnessTheories = [
  {
    title: "7th Dimension",
    description: "Meta-curvature and the configuration space of all possible universes",
    link: "/seventh-dimension",
    icon: <Mountain className="text-dark-pink" size={28} />,
    isNew: true
  },
  {
    title: "8th Dimension",
    description: "The Field of All Possible Consciousnesses",
    link: "/eighth-dimension",
    icon: <Flower className="text-dark-pink" size={28} />,
    isNew: true
  },
  {
    title: "9th Dimension",
    description: "The Meta-Law Field: Source code of consciousness and reality",
    link: "/ninth-dimension",
    icon: <Trees className="text-dark-pink" size={28} />,
    isNew: true
  },
  {
    title: "10th Dimension",
    description: "The Singularity of Totality: Where all distinctions dissolve",
    link: "/tenth-dimension",
    icon: <Infinity className="text-dark-pink" size={28} />,
    isNew: true
  },
  {
    title: "The 10th Door",
    description: "A Mystical Treatise on the Singularity of All Being",
    link: "/tenth-door",
    icon: <DoorOpen className="text-dark-pink" size={28} />,
    isNew: true
  },
  {
    title: "Dimension 0",
    description: "The First Flicker: The Origin Point of All Dimensions",
    link: "/dimension-zero",
    icon: <Sprout className="text-dark-pink" size={28} />,
    isNew: true
  },
  {
    title: "Dream Traversal",
    description: "Simulation of consciousness traversing 6D-8D dream space",
    link: "/dream-traversal",
    icon: <TrainTrack className="text-dark-pink" size={28} />,
    isNew: true
  },
  {
    title: "Unified Consciousness",
    description: "Mathematical formalization of reality as projection of consciousness",
    link: "/unified-consciousness",
    icon: <Gem className="text-dark-pink" size={28} />,
    isNew: true
  },
  {
    title: "Unified Physics & Consciousness",
    description: "Merging infinite consciousness with anti-gravity dark matter theory",
    link: "/unified-physics-consciousness",
    icon: <Infinity className="text-dark-pink" size={28} />,
    isNew: true
  }
];

const container = {
  hidden: { opacity: 0 },
  show: {
    opacity: 1,
    transition: {
      staggerChildren: 0.1
    }
  }
};

const item = {
  hidden: { opacity: 0, y: 20 },
  show: { opacity: 1, y: 0 }
};

export default function DocsPage() {
  return (
    <div className="min-h-screen pt-24 pb-16 px-4">
      <div className="container mx-auto">
        <div className="max-w-4xl mx-auto mb-12 text-center">
          <h1 className="text-4xl font-bold mb-6 text-white neon-glow">Theory Explorer</h1>
          <p className="text-white/80 text-lg">
            Explore different theoretical frameworks that propose modifications to
            gravity as an alternative to dark matter, as well as explorations of consciousness and higher dimensions.
          </p>
        </div>
        
        <div className="mb-16">
          <h2 className="text-3xl font-semibold text-white mb-8 text-center">Physics Theories</h2>
          <motion.div 
            className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6 mt-8"
            variants={container}
            initial="hidden"
            animate="show"
          >
            {theories.map((theory, i) => (
              <motion.div key={i} variants={item}>
                <Link href={theory.link} className="block h-full">
                  <Card className="bg-black border-dark-pink/20 hover:border-dark-pink transition-colors h-full neon-border">
                    <CardHeader>
                      <div className="mb-2">{theory.icon}</div>
                      <CardTitle className="text-white">{theory.title}</CardTitle>
                    </CardHeader>
                    <CardContent>
                      <CardDescription className="text-white/70">{theory.description}</CardDescription>
                    </CardContent>
                    <CardFooter>
                      <p className="text-dark-pink text-sm">Explore Theory &rarr;</p>
                    </CardFooter>
                  </Card>
                </Link>
              </motion.div>
            ))}
          </motion.div>
        </div>
        
        <div>
          <h2 className="text-3xl font-semibold text-white mb-8 text-center">Consciousness & Higher Dimensions</h2>
          <motion.div 
            className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6 mt-8"
            variants={container}
            initial="hidden"
            animate="show"
          >
            {consciousnessTheories.map((theory, i) => (
              <motion.div key={i} variants={item}>
                <Link href={theory.link} className="block h-full">
                  <Card className="bg-black border-dark-pink/20 hover:border-dark-pink transition-colors h-full neon-border">
                    <CardHeader>
                      <div className="mb-2">{theory.icon}</div>
                      <CardTitle className="text-white flex items-center">
                        {theory.title}
                        {theory.isNew && (
                          <span className="ml-2 px-1.5 py-0.5 text-xs bg-dark-pink/20 rounded text-dark-pink">
                            New
                          </span>
                        )}
                      </CardTitle>
                    </CardHeader>
                    <CardContent>
                      <CardDescription className="text-white/70">{theory.description}</CardDescription>
                    </CardContent>
                    <CardFooter>
                      <p className="text-dark-pink text-sm">Explore Theory &rarr;</p>
                    </CardFooter>
                  </Card>
                </Link>
              </motion.div>
            ))}
          </motion.div>
        </div>
      </div>
    </div>
  );
} 