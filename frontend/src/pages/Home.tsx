import React from 'react'
import { motion } from 'framer-motion'

export default function HomePage() {
  return (
    <div className="min-h-screen bg-gradient-to-br from-pink-500 via-red-500 to-yellow-500 flex flex-col items-center justify-center text-white p-6">
      <motion.h1
        className="text-6xl font-extrabold mb-4 text-center"
        initial={{ opacity: 0, y: -20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.8 }}
      >
        VeriScript
      </motion.h1>
      <motion.p
        className="text-xl mb-8 max-w-xl text-center"
        initial={{ opacity: 0 }}
        animate={{ opacity: 1 }}
        transition={{ delay: 0.5, duration: 0.8 }}
      >
        Instantly verify your Cardano contracts with source-on-chain attestation and trust in every transaction.
      </motion.p>
      <motion.button
        className="bg-white text-pink-500 font-bold py-3 px-8 rounded-lg shadow-lg hover:shadow-2xl transform hover:scale-105 transition-all"
        initial={{ opacity: 0 }}
        animate={{ opacity: 1 }}
        transition={{ delay: 1, duration: 0.8 }}
        onClick={() => {
          // navigate to documentation or get started
          window.location.href = '/list'
        }}
      >
        Get Started
      </motion.button>
      <motion.div
        className="absolute bottom-0 left-0 w-full h-32 bg-gradient-to-t from-black via-transparent"
        initial={{ opacity: 0 }}
        animate={{ opacity: 0.5 }}
        transition={{ delay: 1.5, duration: 1 }}
      />
    </div>
  )
}
