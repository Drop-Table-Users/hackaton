/* eslint-disable @typescript-eslint/no-unused-vars */
import React, { useCallback, useEffect, useRef, useState } from 'react'
import { motion } from 'framer-motion'
import { useWallet } from '../hooks/useWallet'
import ConnectButton from '../components/ConnectButton'
import WalletConnect from '../components/WalletConnect'
import type { ProofForm } from '../types/types'
import ProofButton from '../components/ProofButton'
import type { Lucid } from '@lucid-evolution/lucid'

export default function ProofPage() {
  const { isConnected, usedAddresses, initLucid } = useWallet()
  const [form, setForm] = useState<ProofForm>({
    title: '',
    source: '',
    type: 'Validator',
    scriptHash: '',
    address: '',
    policyId: '',
    stakeKey: '',
  })
  const [walletAddress, setWalletAddress] = useState<string | null>(null)
  const hasLoggedAddress = useRef(false)
  const [lucidInstance , setLucidInstance] = useState<Awaited<ReturnType<typeof Lucid>> | null>(null)

  const initialize = useCallback(async () => {
    try {
      const instance = await initLucid()
      if (instance) {
        setLucidInstance(instance)
        if (usedAddresses?.[0] && usedAddresses[0] !== walletAddress) {
          setWalletAddress(usedAddresses[0])
          if (!hasLoggedAddress.current) {
            console.log('Connected wallet address:', usedAddresses[0])
            hasLoggedAddress.current = true
          }
        }
      }
    } catch (error) {
      console.error('Error initializing Lucid:', error)
    }
  }, [initLucid, walletAddress, usedAddresses])

  useEffect(() => {
    if (isConnected) initialize()
    else {
      setWalletAddress(null)
      setLucidInstance(null)
      hasLoggedAddress.current = false
    }
  }, [isConnected, initialize])

  const handleChange = (
    e: React.ChangeEvent<HTMLInputElement | HTMLSelectElement>
  ) => {
    setForm({ ...form, [e.target.name]: e.target.value })
  }

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault()
    console.log('Submitting proof:', form)
    // TODO: integrate proof creation logic
  }

  return (
    <div className="min-h-screen bg-gradient-to-r from-purple-600 via-blue-500 to-teal-400">
      {/* Translucent Navbar */}
      <nav className="fixed top-0 left-0 w-full bg-white bg-opacity-20 backdrop-blur-md shadow-md z-10">
        <div className="max-w-7xl mx-auto px-8 py-3 flex items-center justify-end gap-4">
          <motion.button
            className="bg-white bg-opacity-30 text-white font-semibold py-2 px-4 rounded-md hover:scale-105 transform transition"
            initial={{ opacity: 0, y: -10 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.6 }}
            onClick={() => (window.location.href = '/')}
          >
            Back to List
          </motion.button>
          <ConnectButton />
        </div>
      </nav>

      {/* Form Container */}
      <div className="pt-16 px-8 flex justify-center">
        <div className="w-full max-w-lg bg-white bg-opacity-90 rounded-lg shadow-lg p-6">
          <h1 className="text-2xl font-bold text-gray-800 mb-4 text-center">
            Create Proof
          </h1>
          <form onSubmit={handleSubmit} className="space-y-4">
            <div>
              <label className="block text-gray-700 mb-1">Title</label>
              <input
                type="text"
                name="title"
                value={form.title}
                onChange={handleChange}
                className="w-full border border-gray-300 rounded p-2"
                required
              />
            </div>

            <div>
              <label className="block text-gray-700 mb-1">Source</label>
              <input
                type="text"
                name="source"
                value={form.source}
                onChange={handleChange}
                className="w-full border border-gray-300 rounded p-2"
                placeholder="e.g. GitHub URL"
                required
              />
            </div>

            <div>
              <label className="block text-gray-700 mb-1">Type</label>
              <select
                name="type"
                value={form.type}
                onChange={handleChange}
                className="w-full border border-gray-300 rounded p-2"
              >
                <option value="Validator">Validator</option>
                <option value="Policy">Policy</option>
                <option value="Staking">Staking</option>
              </select>
            </div>

            <div>
              <label className="block text-gray-700 mb-1">Script Hash</label>
              <input
                type="text"
                name="scriptHash"
                value={form.scriptHash}
                onChange={handleChange}
                className="w-full border border-gray-300 rounded p-2"
                required
              />
            </div>

            {/* Conditional Fields */}
            {form.type === 'Validator' && (
              <div>
                <label className="block text-gray-700 mb-1">Address</label>
                <input
                  type="text"
                  name="address"
                  value={form.address}
                  onChange={handleChange}
                  className="w-full border border-gray-300 rounded p-2"
                  required
                />
              </div>
            )}

            {form.type === 'Policy' && (
              <div>
                <label className="block text-gray-700 mb-1">Policy ID</label>
                <input
                  type="text"
                  name="policyId"
                  value={form.policyId}
                  onChange={handleChange}
                  className="w-full border border-gray-300 rounded p-2"
                  required
                />
              </div>
            )}

            {form.type === 'Staking' && (
              <div>
                <label className="block text-gray-700 mb-1">Stake Key</label>
                <input
                  type="text"
                  name="stakeKey"
                  value={form.stakeKey}
                  onChange={handleChange}
                  className="w-full border border-gray-300 rounded p-2"
                  required
                />
              </div>
            )}

            <ProofButton lucidInstance={lucidInstance} form={form}/>
          </form>
        </div>
      </div>

      <WalletConnect />
    </div>
  )
}
