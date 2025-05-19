/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  eslint: {
    // Ignore linting errors during production build
    ignoreDuringBuilds: true,
  },
};

module.exports = nextConfig; 