/* eslint-disable @typescript-eslint/no-unused-vars */
import React, { useCallback, useEffect, useRef, useState } from 'react'
import { ArrowUp, ArrowDown } from 'lucide-react'
import type { Lucid } from '@lucid-evolution/lucid'
import { useWallet } from '../hooks/useWallet'
import WalletConnect from '../components/WalletConnect'
import ConnectButton from '../components/ConnectButton'
import AttestButton from '../components/AttestButton'
import CounterAttestButton from '../components/CounterAttestButton'
import MintSignerButton from '../components/MintSignerButton'
import { motion } from 'framer-motion'
import type { AttestationRow } from '../types/types'
import { fetchAttestationRows } from '../api/onchain'
import { attestationValidatorAddress, counterAttestationValidatorAddress } from '../types/contracts'
import { hexToString } from '../utils/convert'

export default function ListPage() {
  const { isConnected, usedAddresses, initLucid } = useWallet()
  const hasLoggedAddress = useRef(false)
  const [walletAddress, setWalletAddress] = useState<string | null>(null)
  const [lucidInstance , setLucidInstance] = useState<Awaited<ReturnType<typeof Lucid>> | null>(null)
  const [rows, setRows] = useState<AttestationRow[]>([])
  const [loading, setLoading] = useState(true)

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
    console.log("ATT")
    console.log(attestationValidatorAddress)
    console.log(counterAttestationValidatorAddress)
    setLoading(true)
    fetchAttestationRows()
      .then(setRows)
      .catch(console.error)
      .finally(() => setLoading(false))
  }, [])

  useEffect(() => {
    if (isConnected) initialize()
    else {
      setWalletAddress(null)
      setLucidInstance(null)
      hasLoggedAddress.current = false
    }
  }, [isConnected, initialize])

  return (
    <div className="min-h-screen bg-gradient-to-r from-purple-600 via-blue-500 to-teal-400">
      {/* Translucent Navbar */}
      <nav className="fixed top-0 left-0 w-full bg-white bg-opacity-20 backdrop-blur-md shadow-md z-10">
        <div className="max-w-7xl mx-auto px-8 py-3 flex items-center justify-end">
          <motion.button
            className="text-white-500 font-bold py-3 px-8 rounded-lg shadow-lg hover:shadow-2xl transform hover:scale-105 transition-all"
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
            transition={{ delay: 1, duration: 0.8 }}
            onClick={() => {
              window.location.href = '/proof'
            }}
          >
            Create Proof
          </motion.button>
          <MintSignerButton lucidInstance={lucidInstance} />
          <ConnectButton />
        </div>
      </nav>

      {/* Page Content */}
      <div className="pt-16 p-8">
        <h1 className="text-4xl font-extrabold text-white mb-6">Contract Attestations</h1>
        <div className="bg-white rounded-lg shadow-lg overflow-x-auto">
          <table className="min-w-full">
            <thead className="bg-gradient-to-r from-purple-600 via-blue-500 to-teal-400 text-white">
              <tr>
                <th className="py-2 px-3 text-left">Name</th>
                <th className="py-2 px-3 text-left">Type</th>
                <th className="py-2 px-3 text-left">Hash</th>
                <th className="py-2 px-3 text-left">Source</th>
                <th className="py-2 px-3 text-center">Attestations</th>
                <th className="py-2 px-3 text-center">Attest</th>
                <th className="py-2 px-3 text-center">Counter-Attest</th>
              </tr>
            </thead>
            <tbody>
              {rows.map((row) => (
                <tr key={row.hash} className="border-b hover:bg-gray-50 transition">
                  <td className="py-1 px-3 font-medium text-gray-800">{hexToString(row.name)}</td>
                  <td className="py-1 px-3 font-medium text-gray-800">{row.type}</td>
                  <td className="py-1 px-3 font-mono text-sm text-gray-600 truncate max-w-xs">{hexToString(row.hash)}</td>
                  <td className="py-1 px-3">
                    <a
                      href={row.github}
                      target="_blank"
                      rel="noopener noreferrer"
                      className="text-blue-500 hover:underline"
                    >
                      GitHub
                    </a>
                  </td>
                  <td className="py-1 px-3 text-center flex items-center justify-center text-green-500">
                    <ArrowUp size={16} className="mr-1" />
                    {row.upvotes}
                  </td>
                  <td className="py-1 px-3 text-center flex items-center justify-center text-red-500">
                    <ArrowDown size={16} className="mr-1" />
                    {row.downvotes}
                  </td>
                  <td className="py-1 px-3">
                    <AttestButton lucidInstance={lucidInstance} row={row} />
                  </td>
                  <td className="py-1 px-3">
                    <CounterAttestButton lucidInstance={lucidInstance} row={row} />
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
        <WalletConnect />
      </div>
    </div>
  )
}
