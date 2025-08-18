# Meetdao - Decentralized Meeting Governance

A Clarity smart contract for creating and managing public meetings with proposal submission and voting mechanisms on the Stacks blockchain.

## Features

- **Meeting Management**: Create, schedule, and manage meetings
- **Proposal System**: Submit agenda items with deposits and voting periods
- **Voting Mechanism**: Weighted voting with yes/no/abstain options
- **Governance**: Owner controls for voting parameters and user permissions
- **Attendance Tracking**: Join meetings and track participation

## Contract Functions

### Public Functions

#### Meeting Management
- `create-meeting(title, description, scheduled-block, duration-blocks)` - Create a new meeting
- `join-meeting(meeting-id)` - Join an active meeting
- `cancel-meeting(meeting-id)` - Cancel a scheduled meeting (organizer only)

#### Proposal Management
- `create-proposal(title, description, meeting-id, execution-delay)` - Submit a proposal with STX deposit
- `vote-on-proposal(proposal-id, vote-type)` - Vote on proposals (1=yes, 2=no, 3=abstain)
- `finalize-proposal(proposal-id)` - Finalize voting after end block

#### Administrative
- `set-voting-power(user, power)` - Set user voting weight (owner only)
- `update-voting-duration(duration)` - Update default voting period (owner only)
- `update-proposal-deposit(amount)` - Update minimum proposal deposit (owner only)

### Read-Only Functions

- `get-proposal(proposal-id)` - Get proposal details
- `get-meeting(meeting-id)` - Get meeting information
- `get-vote(proposal-id, voter)` - Get specific vote details
- `get-proposal-results(proposal-id)` - Get voting results and percentages
- `get-user-voting-power(user)` - Get user's voting weight
- `is-proposal-active(proposal-id)` - Check if proposal is actively accepting votes
- `can-user-vote(proposal-id, user)` - Check if user can vote on proposal
- `get-contract-info()` - Get contract configuration and statistics

## Usage Examples

### 1. Create a Meeting
```clarity
(contract-call? .meetdao create-meeting 
    u"Weekly Standup" 
    u"Discuss project progress and blockers" 
    u1000 
    u144)
```

### 2. Submit a Proposal
```clarity
(contract-call? .meetdao create-proposal 
    u"Budget Allocation" 
    u"Allocate 10K STX for marketing campaign" 
    u1 
    u72)
```

### 3. Vote on Proposal
```clarity
(contract-call? .meetdao vote-on-proposal u1 u1)
```

### 4. Join Meeting
```clarity
(contract-call? .meetdao join-meeting u1)
```

### 5. Check Proposal Results
```clarity
(contract-call? .meetdao get-proposal-results u1)
```

## Configuration

- **Minimum Voting Duration**: 144 blocks (~24 hours)
- **Maximum Voting Duration**: 4320 blocks (~30 days)
- **Default Voting Duration**: 1008 blocks (~7 days)
- **Minimum Quorum**: 10% participation
- **Proposal Execution Threshold**: 51% yes votes

## Error Codes

- `u100` - Not authorized
- `u101` - Proposal not found
- `u102` - Proposal inactive
- `u103` - Already voted
- `u104` - Voting period ended
- `u105` - Insufficient votes
- `u106` - Proposal not executable
- `u107` - Invalid duration
- `u108` - Proposal already finalized
- `u109` - Meeting not found
- `u110` - Invalid meeting status

## Development

This contract is built for the Stacks blockchain using Clarity language. Deploy using Clarinet development environment.

### Testing
```bash
clarinet test
```

### Deployment
```bash
clarinet deploy
```

## License

MIT License
