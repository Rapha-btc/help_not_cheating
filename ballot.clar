;; let's ask for help to peers to review this
;; voting

;; constants
;;
(define-constant ERR_UNAUTHORIZED (err u100));;unauthorized
(define-constant ERR_INVALID_VOTE_EXPIRY (err u101)) ;; expired date?
(define-constant ERR_BALLOT_NOT_FOUND (err u102));; ballot not found
(define-constant ERR_CANDIDATE_NOT_FOUND (err u103));; no such candidate
(define-constant ERR_VOTED_ALREADY (err u104)) ;; u can't vote twice
(define-constant ERR_VOTING_ENDED (err u105)) ;; too late sir
(define-constant ERR_CANNOT_END_BEFORE_EXPIRY (err u106)) ;;expiration is expiration not before
;; data maps and vars
;;
(define-data-var ballotId uint u0) ;;BallotId is an uint intitialized at u0

(define-map Ballot uint { ;;let's define the map Ballot number uint - takes name, starts, ends, totalVotes, status, 10 candidates and owner
        name: (string-ascii 50), 
        startsAt: uint, 
        endsAt: uint, 
        totalVotes: uint, 
        status: (string-ascii 10),
        candidates: (list 10 { name: (string-ascii 50), address: principal }),
        owner: principal
    }
)

(define-map CandidateVotes { address: principal, ballotId: uint } { votes: uint, name: (string-ascii 50) })
;; candidatevotes who voted and with what number ballot, number of votes and who
(define-map Voters { address: principal, ballotId: uint } bool)
;; Votes who and the Id of their ballot true or false

;; private functions
;; 

;; sets the candidates with unique key of their address and the ballot id to tally the total votes
(define-private (init-candidates (candidate { name: (string-ascii 50), address: principal }))
;;private function init-candidates
    (map-set CandidateVotes 
                { address: (get address candidate), ballotId: (+ (var-get ballotId) u1) } 
                { votes: u0, name: (get name candidate) }))

                ;; okay so this function updates CandidateVotes, 
                ;; input a candidate defined by name / address
                ;; output is a blind update of CandidateVotes: address / ballot id +1
                ;; output is a blind update of CandidateVotes: initialized to u0 votes / name

                ;; give me a list candidate of a name and a principal, I'll add it to the map of candidatesAndtheirVotecount
                ;; next candidate has ballot id + 1
                

;; map-set performs a blind update regardless of there's a value with key or not

;; public functions
;;

;; @function start-vote: will start valid ballot
;; 1. Any user can start a vote
;; 2. Validates that the vote ends at a block in the future creates a new Ballot with information about the vote
;; 3. initializes the candidates
;; 4. Sets the vote related info on the ballot
;; 5. updates the ballot id 
;; @param name:string :: title describing what this vote is for. 
;; @param endsAt:uint :: when does it end
;; @param candidates:list of tuples :: list of names and wallets you can vote for
;; @response (ok uint) Return the lastest ballot id in the response
(define-public (start-vote
                    (name (string-ascii 50))
                    (endsAt uint)
                    (candidates (list 10 { name: (string-ascii 50), address: principal } ))
                ) ;;params: name or what is this vote for // ends when // 10 candidates
                    (let ((newBallotId (+ (var-get ballotId) u1) )) ;;newId = old + 1 - this is starting a new ballot so +1
                        (asserts! (> endsAt block-height) ERR_INVALID_VOTE_EXPIRY);;if ends > now good else invalid
;;I forgot what let is for -> it's for having newBallotId as a local variable in the contract
;; being able to reuse it, separately, per function
                        (map init-candidates candidates) ;; map takes a function- almost like calling function on each element of list
                        (map-set Ballot newBallotId { ;;newballotID is u1
                                name: name, 
                                startsAt: block-height, 
                                endsAt: endsAt, 
                                totalVotes: u0, 
                                status: "Active",
                                candidates: candidates,;;all 10 candidates on the Ballot
                                owner: tx-sender
                            })
                        (var-set ballotId newBallotId);; here I update the BallotID now that I finished starting the vote/Ballot
                        (ok newBallotId)
                    )
)
;; startsAt is just when the ballot is active
;; ballot is active now and this is the ballot
;; spits the last ballot number

;; @function vote: allow users to vote for the ballot



;; @param id:uint :: ballot id for which you want to vote
;; @param choice:principal 

(define-public (vote (id uint) (choice principal)) 
    ;; IMPLEMENT HERE
    (let
        (
            (itEnded (get endsAt (unwrap! (map-get? Ballot id) ERR_BALLOT_NOT_FOUND)))
            (NumberVotes (get totalVotes (unwrap! (map-get? Ballot id) ERR_BALLOT_NOT_FOUND)))
            (NumberPlus1 (+ NumberVotes u1))
            (voteCount (get votes (unwrap! (map-get? CandidateVotes { address: choice, ballotId: id } ) ERR_BALLOT_NOT_FOUND )))
            (voteCount1 (+ voteCount u1))
        )
            
    ;; 1. Any user can vote for the candidate only once. ;; user address must be absent in candidateVotes
            (asserts! (is-none (map-get? CandidateVotes { address: choice, ballotId: id })) ERR_VOTED_ALREADY)
    ;; 2. Validates that the the voter has not already voted ;; user address must be absent in voters map or false
            (asserts! (is-none (map-get? Voters { address: choice, ballotId: id })) ERR_VOTED_ALREADY);; i wouldn't call choice choice, more like person who votes
    ;; 3. Validates that the vote is not yet ended
            (asserts! (< block-height itEnded) ERR_VOTING_ENDED)
    ;; 4. updates the voter info who has voted ;; map setting voters
            (map-set Voters { address: choice, ballotId: id } true)
    ;; 5. updates the total count on the ballot ;; update ballot # ballotID of candidate
            (merge (unwrap! (map-get? Ballot id) ERR_BALLOT_NOT_FOUND) {totalVotes: NumberPlus1})
    ;; 6. updates the total count for a candidate ;; update candidatevotes
            (merge (unwrap! (map-get? CandidateVotes { address: choice, ballotId: id } ) ERR_CANDIDATE_NOT_FOUND) { votes: voteCount1} )
            
            (ok true)
    )
)

(define-public (end-vote (id uint))
    (let 
        (
            (itEnded (get endsAt (unwrap! (map-get? Ballot id) ERR_BALLOT_NOT_FOUND)))
        )
        ;; 1. Only the owner/admin can end vote
        (asserts! (is-eq (as-contract tx-sender) tx-sender) ERR_UNAUTHORIZED);;only contract owner
        ;; 2. cannot end before expiry is reached
        (asserts! (> block-height itEnded) ERR_CANNOT_END_BEFORE_EXPIRY)

        ;; 3. deletes the ballot from the map
        (map-delete Ballot id)
        ;; @param id:uint :: ballot id which you want to end
        ;; @response (ok bool) Returns true when ballot status is updated successfully to "Ended"
        (ok true)
    )
)

;; read-only functions
;;
(define-read-only (candidate-info (id uint) (candidate principal)) 
    (map-get? CandidateVotes { address: candidate, ballotId: id }))

(define-read-only (voter-info (id uint) (voter principal)) 
    (map-get? Voters { address: voter, ballotId: id }))

(define-read-only (vote-info (id uint)) 
    (map-get? Ballot id))

;; Making contract calls from console:

;; 1. Start a vote
;; (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.ballot start-vote "Vote 1" u20 (list {name: "A", address: 'ST3NBRSFKX28FQ2ZJ1MAKX58HKHSDGNV5N7R21XCP} {name: "B", address: 'STNHKEPYEPJ8ET55ZZ0M5A34J0R3N5FM2CMMMAZ6 }))

;; 2. Give a vote
;; (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.ballot vote u1 'ST3PF13W7Z0RRM42A8VZRVFQ75SV1K26RXEP8YGKJ)

;; 3. End a vote
;; (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.ballot end-vote u1)

;; 4. Read ballot info 
;; (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.ballot vote-info u1)

;; 5. Read first candidate 
;; (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.ballot candidate-info 'ST3NBRSFKX28FQ2ZJ1MAKX58HKHSDGNV5N7R21XCP u1)

;; 6. Read second candidate 
;; (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.ballot candidate-info 'STNHKEPYEPJ8ET55ZZ0M5A34J0R3N5FM2CMMMAZ6 u1)
