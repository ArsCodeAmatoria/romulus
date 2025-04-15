'use client';

import * as React from 'react';
import Link from 'next/link';
import { usePathname } from 'next/navigation';
import { Github } from 'lucide-react';

import {
  NavigationMenu,
  NavigationMenuContent,
  NavigationMenuItem,
  NavigationMenuLink,
  NavigationMenuList,
  NavigationMenuTrigger,
} from '@/components/ui/navigation-menu';

export default function Navigation() {
  const pathname = usePathname();
  
  return (
    <div className="fixed top-0 left-0 right-0 z-50 bg-black/20 backdrop-blur-md border-b border-dark-pink/20">
      <div className="container flex h-16 items-center justify-between px-4">
        <NavigationMenu>
          <NavigationMenuList>
            <NavigationMenuItem>
              <Link 
                href="/" 
                className={`text-xl font-bold neon-glow ${pathname === '/' ? 'text-dark-pink' : 'text-white'}`}
              >
                ROMULUS
              </Link>
            </NavigationMenuItem>
            
            <NavigationMenuItem>
              <NavigationMenuTrigger className={`${pathname.startsWith('/docs') ? 'text-dark-pink' : 'text-white'}`}>
                Theories
              </NavigationMenuTrigger>
              <NavigationMenuContent>
                <ul className="grid w-[400px] gap-3 p-4 md:w-[500px] md:grid-cols-2">
                  <li>
                    <Link 
                      href="/docs/general-relativity" 
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none">General Relativity</div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        Tensor modifications to Einstein's field equations
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/docs/emergent-gravity"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none">Emergent Gravity</div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        Verlinde's theory of gravity as an entropic force
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/docs/mond"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none">MOND & TeVeS</div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        Modified Newtonian Dynamics and Tensor-Vector-Scalar gravity
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/docs/scalar-field"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none">Scalar Field Gravity</div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        f(R) and scalar field approaches to modified gravity
                      </p>
                    </Link>
                  </li>
                </ul>
              </NavigationMenuContent>
            </NavigationMenuItem>
            
            <NavigationMenuItem>
              <Link 
                href="/docs" 
                className={`${pathname === '/docs' ? 'text-dark-pink' : 'text-white'}`}
              >
                Documentation
              </Link>
            </NavigationMenuItem>
            
            <NavigationMenuItem>
              <Link 
                href="/interactive-demo" 
                className={`${pathname === '/interactive-demo' ? 'text-dark-pink' : 'text-white'} flex items-center`}
              >
                <span className="mr-2">Interactive</span>
                <span className="px-1.5 py-0.5 text-xs bg-dark-pink/20 rounded text-dark-pink">New</span>
              </Link>
            </NavigationMenuItem>
          </NavigationMenuList>
        </NavigationMenu>
        
        <a 
          href="https://github.com/ArsCodeAmatoria/romulus" 
          target="_blank" 
          rel="noopener noreferrer"
          className="text-white hover:text-dark-pink transition-colors"
          aria-label="View source on GitHub"
        >
          <Github size={24} />
        </a>
      </div>
    </div>
  );
} 