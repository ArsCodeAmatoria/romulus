import Link from "next/link";
import { Button } from "@/components/ui/button";
import { motion } from "framer-motion";

export default function AboutPage() {
  return (
    <div className="min-h-screen pt-24 pb-16 px-4">
      <div className="container mx-auto">
        <div className="max-w-3xl mx-auto mb-12">
          <h1 className="text-4xl font-bold mb-6 text-white neon-glow text-center">About Romulus</h1>
          
          <section className="mb-12">
            <h2 className="text-2xl font-semibold text-dark-pink mb-4">The Boundary Condition Theory</h2>
            <p className="text-white/80 mb-4">
              What if dark matter is not a substance but rather the "outline" of the cosmic painting? This novel perspective suggests that the gravitational effects we attribute to dark matter might actually emerge from how gravity behaves at the boundaries or edges of matter concentrations.
            </p>
            <p className="text-white/80 mb-4">
              By modifying Einstein's equations with additional scalar fields or geometric terms, we can reproduce dark matter effects without requiring new particles. This approach could explain why direct detection experiments have consistently failed to find dark matter particles despite strong indirect evidence for their gravitational effects.
            </p>
          </section>
          
          <section className="mb-12">
            <h2 className="text-2xl font-semibold text-dark-pink mb-4">Our Mission</h2>
            <p className="text-white/80 mb-4">
              Romulus is an educational platform exploring alternative explanations for dark matter phenomena through modified gravity theories. The project provides interactive visualizations and in-depth explanations of different theoretical frameworks that challenge the standard dark matter particle model.
            </p>
            <p className="text-white/80 mb-4">
              Our featured perspective reimagines dark matter not as discrete particles, but as a boundary condition or structural framework that defines how visible matter behaves—similar to how an artist outlines figures before filling in with color.
            </p>
          </section>
          
          <section className="mb-12">
            <h2 className="text-2xl font-semibold text-dark-pink mb-4">Theories We Explore</h2>
            <ul className="list-disc list-inside text-white/80 space-y-2 mb-6">
              <li>General Relativity + Effective Tensor Modifications</li>
              <li>Emergent Gravity (Verlinde)</li>
              <li>MOND and TeVeS</li>
              <li>Scalar Field / f(R) Gravity</li>
              <li>Quantum Gravity + Holographic Principle</li>
              <li>Dark Matter as Boundary Condition</li>
            </ul>
            <Button 
              className="bg-dark-pink hover:bg-dark-pink/80 text-white neon-border"
              asChild
            >
              <Link href="/docs">
                Explore All Theories
              </Link>
            </Button>
          </section>
          
          <section className="mb-12">
            <h2 className="text-2xl font-semibold text-dark-pink mb-4">Tech Stack</h2>
            <p className="text-white/80 mb-4">
              Built with modern web technologies to provide an immersive educational experience:
            </p>
            <ul className="list-disc list-inside text-white/80 space-y-2">
              <li>Next.js with App Router</li>
              <li>Tailwind CSS</li>
              <li>shadcn/ui components</li>
              <li>Three.js with @react-three/fiber and @react-three/drei</li>
              <li>Framer Motion for animations</li>
              <li>KaTeX for LaTeX math rendering</li>
            </ul>
          </section>
          
          <div className="flex justify-center mt-8">
            <Button 
              variant="outline" 
              className="border-dark-pink text-dark-pink hover:bg-dark-pink/20 neon-border"
              asChild
            >
              <Link href="/">
                Back to Home
              </Link>
            </Button>
          </div>
        </div>
      </div>
    </div>
  );
} 