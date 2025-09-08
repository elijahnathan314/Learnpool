# 🎓 Learnpool - Peer Learning DAO

> A decentralized autonomous organization where learners collectively vote and fund their curriculum through community governance.

## 🌟 Overview

Learnpool empowers learning communities to democratically decide what educational content gets funded and created. Members stake STX tokens to join the DAO, propose learning initiatives, vote on proposals, and collectively fund approved educational projects.

## ✨ Key Features

- 🏛️ **Democratic Governance**: Stake-weighted voting system for fair decision making
- 💰 **Community Treasury**: Pooled funds managed by the community
- 📚 **Curriculum Proposals**: Members can propose and fund learning initiatives  
- 🗳️ **Transparent Voting**: Open voting process with configurable periods
- 💎 **Flexible Staking**: Join with any amount, earn voting power based on stake
- 🚪 **Exit Mechanism**: Leave the DAO and withdraw your stake anytime

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- STX tokens for staking and participation

### Installation

```bash
git clone <repository-url>
cd learnpool
clarinet check
```

## 📖 Usage Guide

### 1. 🎯 Join the DAO

Stake STX tokens to become a member and gain voting power:

```clarity
(contract-call? .learnpool join-dao u5000000) ;; Stake 5 STX
```

### 2. 📝 Create Learning Proposals

Propose educational initiatives that need funding:

```clarity
(contract-call? .learnpool create-proposal 
  "Advanced Smart Contracts Course" 
  "Comprehensive course covering advanced Clarity programming patterns and best practices"
  u10000000) ;; Request 10 STX funding
```

### 3. 🗳️ Vote on Proposals

Cast your vote on active proposals:

```clarity
(contract-call? .learnpool vote-on-proposal u1 true) ;; Vote YES on proposal #1
```

### 4. ✅ Finalize Proposals

After voting period ends, finalize to execute results:

```clarity
(contract-call? .learnpool finalize-proposal u1)
```

### 5. 💸 Withdraw Approved Funds

Proposal creators can withdraw approved funding:

```clarity
(contract-call? .learnpool withdraw-funds u1 u5000000) ;; Withdraw 5 STX
```

## 🔍 Read-Only Functions

Query contract state without transactions:

```clarity
;; Get proposal details
(contract-call? .learnpool get-proposal u1)

;; Check member stake
(contract-call? .learnpool get-member-stake 'SP1234...)

;; View treasury balance
(contract-call? .learnpool get-treasury-balance)

;; Check voting power
(contract-call? .learnpool get-voting-power 'SP1234...)
```

## ⚙️ Configuration

### Voting Parameters

- **Voting Period**: 1440 blocks (~10 days)
- **Minimum Proposal**: 1 STX
- **Approval Threshold**: >50% of votes cast

### Admin Functions

Contract owner can adjust parameters:

```clarity
;; Update voting period
(contract-call? .learnpool update-voting-period u2880) ;; ~20 days

;; Update minimum proposal amount  
(contract-call? .learnpool update-min-proposal-amount u2000000) ;; 2 STX minimum
```

## 🏗️ Architecture

### Core Components

- **Member Management**: Staking and membership tracking
- **Proposal System**: Creation, voting, and execution of learning initiatives
- **Treasury Management**: Community fund management and distribution
- **Governance**: Democratic decision-making with stake-weighted voting

### Voting Power Calculation

Voting power scales with stake amount:
- Base power: 10 + (stake / 10,000)
- Large stakes (>10 STX): 100 + (stake / 100,000)

## 🛡️ Security Features

- ✅ Stake-based access control
- ✅ Time-locked voting periods  
- ✅ Single vote per member per proposal
- ✅ Treasury balance validation
- ✅ Creator-only fund withdrawal

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Test with `clarinet test`
4. Submit a pull request

## 📄 License

MIT License - Build the future of decentralized learning! 🚀


