# 🛡️ RiskDAO - Decentralized Insurance Underwriting Protocol

> 🚀 A crowdsourced insurance and liability scoring platform built on Stacks blockchain

## 📋 Overview

RiskDAO is a decentralized autonomous organization that enables crowdsourced insurance underwriting. Community members can stake tokens to evaluate insurance policies and vote on claims, creating a transparent and democratic insurance ecosystem.

## ✨ Key Features

- 🏦 **Decentralized Underwriting**: Community-driven risk assessment
- 💰 **Stake-Based Voting**: Underwriters stake STX to participate in decisions  
- 📊 **Risk Scoring**: Collaborative risk evaluation for insurance policies
- 🗳️ **Claim Voting**: Democratic claim approval process
- 🎯 **Reputation System**: Track underwriter performance over time
- ⏰ **Time-Bound Voting**: Structured voting periods for claims

## 🔧 Core Functions

### For Underwriters
- `register-underwriter` - Stake STX to become an underwriter
- `vote-on-policy` - Evaluate and score insurance policy risk
- `vote-on-claim` - Vote to approve/deny insurance claims

### For Policy Holders  
- `submit-policy` - Apply for insurance coverage
- `submit-claim` - File a claim against your policy
- `finalize-policy` - Activate policy after sufficient underwriter votes

### Administrative
- `process-claim` - Execute claim decision after voting period

## 🚀 Getting Started

### Prerequisites
- Clarinet CLI installed
- Stacks wallet with STX tokens

### Installation

```bash
git clone <repository-url>
cd riskdao
clarinet check
```

### Usage Examples

#### 1️⃣ Register as Underwriter
```clarity
(contract-call? .Riskdao register-underwriter u5000000)
```

#### 2️⃣ Submit Insurance Policy
```clarity
(contract-call? .Riskdao submit-policy u10000000 u500000)
```

#### 3️⃣ Vote on Policy Risk
```clarity
(contract-call? .Riskdao vote-on-policy u1 u75 u1000000)
```

#### 4️⃣ Submit Insurance Claim
```clarity
(contract-call? .Riskdao submit-claim u1 u5000000 "Car accident claim")
```

#### 5️⃣ Vote on Claim
```clarity
(contract-call? .Riskdao vote-on-claim u1 true u2000000)
```

## 📖 How It Works

1. **🏗️ Policy Creation**: Users submit insurance applications with coverage amount and premium
2. **⚖️ Risk Assessment**: Underwriters stake tokens and provide risk scores (0-100)
3. **✅ Policy Activation**: Policies activate after minimum 3 underwriter votes
4. **📝 Claim Submission**: Policy holders can file claims up to coverage amount
5. **🗳️ Claim Voting**: Underwriters vote approve/deny with stake-weighted decisions
6. **💸 Claim Processing**: Claims auto-execute after voting period based on stake majority

## 🔒 Security Features

- Minimum stake requirements for underwriters
- Time-locked voting periods
- Stake-weighted decision making
- One vote per underwriter per policy/claim
- Automated claim processing

## 📊 Read-Only Functions

- `get-policy` - View policy details
- `get-claim` - View claim information  
- `get-underwriter` - Check underwriter stats
- `get-contract-balance` - View total contract funds
- `calculate-average-risk-score`# Riskdao

