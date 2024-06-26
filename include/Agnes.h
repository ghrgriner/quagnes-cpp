//  quagnes: A program for solving Agnes solitaire
//   Copyright (C) 2019, 2024 Ray Griner (rgriner_fwd@outlook.com)
//
//   This program is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program.  If not, see <https://www.gnu.org/licenses/>.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// File: Agnes.h
// Date: 2019
// Author: Ray Griner
// Purpose: Header file for Agnes class
// Changes:
// [20240114RG]: New `Agnes.max_states_guard_` attribute to guard against
// overflow and macro variable kNStatesMax with the maximum.
// [20240416RG] (1) Switch C-style #define constants to const variables.
// (2) Make mark appropriate functions as const.
// [20240421RG] Add `max_possible_score` stack as attribute.
// [20240422RG] Make `check_loops_` and `losing_states_` unordered sets instead
//   of sets.
// [20240426RG] Add `print_memory` input paramer and attribute.
// [20240520RG] Add `random_seed` and `burn_in` input parameters and attributes.
//   Rename `InitializeDeckFromFile` to `InitializeDeck` and remove all
//   parameters and add logic to generate deck from a random shuffle in addition
//   to reading from file.
//------------------------------------------------------------------------------

#ifndef _QUAGNES_AGNES_H
#define _QUAGNES_AGNES_H

#include <string>
#include <unordered_set>
#include <stack>
#include <cstdint>

#include "AgnesState.h"

namespace quagnes {

//----------------------------------------------------------------------
// Typedef
//----------------------------------------------------------------------

// If changing this, be sure to update the function in quagnes.cpp that
// converts the input argument to a StatesType
typedef uint64_t StatesType;
const StatesType kNStatesMax = UINT64_MAX;

//------------------------------------------------------------------------------
//  AgnesOptions: options to be passed at Agnes() construction
//
//  Attributes
//  ----------
//  move_to_empty_pile : {'none', 'high 1', 'any 1', 'high run', 'any run'}
//      Optional parameter, default is 'none'. Describes which single
//      cards or runs can be moved to an empty pile.
//      'none': (default), no card can be moved from the tableau. Empty
//          piles are only filled when dealing from stock.
//      'any 1': Any single card can be moved.
//      'high 1': Any single card of highest rank can be moved. For
//          example, if the base card is a 3, then a 2 can be moved.
//      'any run': Any movable run can be moved.
//      'high run': Any movable run that is built down from a card of
//          highest rank.
//  move_same_suit : boolean, optional (default = False)
//      If True, only permit moving sequences of cards in the tableau
//      that are the same suit. Otherwise, permit moving sequences of
//      cards that are the same color.
//  split_runs: boolean, optional (default = True)
//      If True, allow a movable run to be split during a move. If
//      false, movable runs must be moved in their entirety.
//  face_up : boolean, optional (default = False)
//      Deal all cards face up in the tableau.
//  track_threshold : int, optional (default = 0)
//      If the number of cards left in the stock is greater than or
//      equal to this value, track losing states in a single set for
//      the whole game. This set can consume a lot of memory if some of
//      the other options are chosen that allow a large number of moves
//      (eg, `move_to_empty_pile != 'none'`).
//  print_states : boolean, optional (default = False)
//      Print game states as moves are made. See `Agnes.print_history`
//      for output format.
//  print_memory : boolean, optional (default = False)
//      Print information useful for estimating memory utilization to
//      standard output.
//  maximize_score : boolean, optional (default = False)
//      Determine the maximum score. Disables the algorithm used when
//      `move_to_empty_pile == 'none'` that stops playing the game
//      when it detects a game is unwinnable.
//  test_deck : {0, 1}, optional (default = 0)
//      If 0, a random deck is generated. If 1, a fixed test deck that
//      wins is used.
//  deck_filename : string, optional
//      Read deck from text file, If empty, a random deck will be
//      used by calling random.shuffle and reversing the results.
//      The text file should consist of 52 lines with each line is
//      formatted as "(rank, suit)", where rank is in 0..12 and
//      suit is in 0..3. Note the first card dealt is the base card.
//  max_states : int, optional
//      Terminate game with return code 3 when number of states
//      examined exceeds this threshold.  0 (default) means no
//      threshold is used.
// random_seed : uint32_t, optional
//      If non-zero, single random seed to be passed to std::mt19937 for
//      randomly shuffling deck instead of reading deck from `deck_filename`.
// burn_in : uint64_t, optional
//      If non-zero, burn-in period to use for std::mt19937 (i.e., call
//      mt19937.discard with this parameter after seeding).
//------------------------------------------------------------------------------
struct AgnesOptions {
  std::string deck_filename = "";
  std::string move_to_empty_pile = "none";
  bool move_same_suit = false;
  bool split_runs = true;
  int track_threshold = 0;
  bool face_up = false;
  bool maximize_score = false;
  bool print_states = false;
  bool print_memory = false;
  StatesType max_states = 0;
  uint32_t random_seed = 0;
  uint64_t burn_in = 0;
};

//-----------------------------------------------------------------------------
// Represents a game of Agnes solitaire
//
//    Attributes
//    ----------
//    n_states_checked : StatesType
//        Number of states examined
//    n_deal : StatesType
//        Number of deals performed
//    n_move_card_in_tableau : StatesType
//        Number of moves of card(s) between piles in tableau
//    n_move_to_foundation : StatesType
//        Number of times a card was moved to foundation
//    n_no_move_possible : StatesType
//        Number of states created where no move was possible
//    max_depth : int
//        Maximum depth of the search tree
//    current_depth : int
//        Current depth of the search tree
//    max_score : int
//        Maximum score obtained (i.e., maximum number of cards moved
//        to the foundations). For the default input parameters, the
//        program backtracks as soon as it detects a state cannot be
//        won. A higher maximum score may be possible if the game were
//        played in full.
//    maximize_score : boolean
//        Stores value of input option with the same name
//    move_to_empty_pile : str
//        Stores value of input option with the same name
//    move_same_suit : boolean
//        Stores value of input option with the same name
//    split_runs : boolean
//        Stores value of input option with the same name
//    face_up : boolean
//        Stores value of input option with the same name
//    maximize_score : boolean
//        Stores value of input option with the same name
//    track_threshold : boolean
//        Stores value of input option with the same name
//    print_states : bool
//        Stores value of input option with the same name
//    print_memory : bool
//        Stores value of input option with the same name
//    test_deck : bool
//        Stores value of input option with the same name
//    deck_filename : bool
//        Stores value of input option with the same name
//    max_states : boolean
//        Stores value of input option with the same name
//
// Example:
//
//   AgnesOptions agnes_options;
//   agnes_options.deck_filename = "input_deck.txt";
//   agnes_options.move_to_empty_pile = "high run";
//   Agnes agnes = Agnes(agnes_options);
//   rc = agnes.Play();
//   if (rc == 1) {std::cout << "The game is winnable!"; }
//-----------------------------------------------------------------------------

class Agnes {
  public:
    Agnes(const AgnesOptions& agnes_options);

    //--------------------------------------------------------------------------
    // Play the game of Agnes and return integer status.
    //
    // Returns
    // -------
    // An integer with the following meanings:
    //     1: Won
    //     2: Lost
    //     3: Terminated because number of states created exceeds
    //        max_states
    //
    // Throws
    // ------
    // std::invalid_argument - move_to_empty_pile not valid
    // FileNotFoundError     - deck_filename provided but not found
    //--------------------------------------------------------------------------
    int Play();

    StatesType n_states_checked() const;
    StatesType n_deal() const;
    StatesType n_move_card_in_tableau() const;
    StatesType n_move_to_foundation() const;
    StatesType n_no_move_possible() const;
    int max_depth() const;
    uint64_t n_losing_states() const;
    std::string move_to_empty_pile() const;
    bool move_same_suit() const;
    bool print_states() const;
    int track_threshold() const;
    bool split_runs() const;
    bool maximize_score() const;
    bool face_up() const;
    int current_depth() const;
    int max_score() const;
    bool print_memory() const;
    Agnes(const Agnes& other) = default;
    Agnes& operator=(const Agnes& other) = default;
    uint64_t cum_length() const;
    std::array<uint64_t, kNSymbol> cum_symbol_count0() const;
    std::array<uint64_t, kNSymbol> cum_symbol_count1() const;

  private:
    std::string deck_filename_;
    std::string move_to_empty_pile_;
    bool move_same_suit_;
    bool split_runs_;
    int track_threshold_;
    bool face_up_;
    bool maximize_score_;
    bool print_states_;
    bool print_memory_;
    StatesType max_states_;
    uint32_t random_seed_;
    uint64_t burn_in_;
    StatesType n_states_checked_;
    StatesType n_deal_;
    StatesType n_move_card_in_tableau_;
    StatesType n_move_to_foundation_;
    StatesType n_no_move_possible_;
    int max_depth_;

    // Stores state of the game that is modified as the game is played, ie,
    // what cards are in which tableau piles and foundation, and how many are
    // left in stock. Also contains additional information for the algorithm
    // optimizations.
    AgnesState curr_state_;
    // Maximum score attained for the game so far
    int max_score_;
    // Current score
    int score_;
    // Current depth in the search tree (ie, how many moves have been played)
    int current_depth_;
    // Deck before normalizing to base card having rank = 0
    Deck initial_deck_;
    // After normalizing deck so that base card has rank = 0
    Deck deck_;
    // Stack of valid moves remaining at each point in the game.
    std::stack<std::vector<Move> > all_valid_moves_;
    // Stack of moves played to reach the current point in the game.
    std::stack<Move> moves_;
    // Stack of last move info at each point in the game. Used for an algorithm
    // optimization.
    std::stack<std::array<LastMoveInfo, kNPile>> all_lmi_;
    // Set containing game states that have been observed in the current path
    // through the game in order to prevent loops.
    std::unordered_set<StateForSet> check_loops_;
    // Set containing game states that are known to be losers. This can get
    // quite large.
    std::unordered_set<StateForSet> losing_states_;

    // convert move_to_empty_pile_ to an enum
    EmptyRule enum_to_empty_pile_;
    // It is not possible to have loops when (move_to_empty_pile="none" and
    // split_runs = false), so set a flag so we can avoid checking.
    bool check_for_loops_;
    // `If move_to_empty_pile` is not 'any 1' or 'high 1' there is no benefit to
    // splitting a run between the same suit for a move.
    bool split_empty_stock_;
    // Array that tracks how many valid moves for the first kNToTrack game
    // states. Periodically printed to stderr so user can check whether a
    // long-running game is likely to finish soon. TODO: consider switching
    // all_valid_moves_ to a vector so this can be done away with.
    std::array<int, kNToTrack> moves_left_;
    // Minimum of max_states_ (if max_states_ > 0) and INT_MAX (if == 0)
    StatesType max_states_guard_;
    // Maximum possible score (52, unless move_to_empty_pile == "none"), in
    // which case it is calculated from which cards are blocked in the
    // tableau. This is a stack with an element for every deal (including the
    // initial deal).
    std::stack<int> max_possible_score_;

    //-------------------------------------------------------------------------
    // Initialize deck, either by a random shuffle of 0..51 or from file
    //
    // If not `random_seed`, initialize deck from random shuffle of 0..51.
    // Otherwise, read deck from input file, one card per line where
    // card='(rank, suit)', and Rank = 0, 1, ... 12 and suit = {0, 1, 2, 3} and
    // suits are the same color if they are equal modulus 2.
    // Return 1 if error, 0 if successful
    //-------------------------------------------------------------------------
    void InitializeDeck();

    //-------------------------------------------------------------------------
    // Perform a move. Calls UndoMove if no possible move.
    //
    // To make a move, update:
    //     (1) the appropriate attributes of `curr_state`
    //         (`exposed`, `hidden`, `n_stock_left`, `foundation`,
    //          `last_in_pile`, `in_suit_seq`)
    //     (2) any counters in self that count the various types of
    //         moves made (`n_move_to_foundation`,`n_deal`,
    //         `n_move_card_in_tableau`).
    //     (3) append to the stacks that track the evolution of the
    //         game:` moves_`, `all_valid_moves_`, `all_lmi_`.
    //
    // Returns
    // -------
    // Integer with the values:
    //    0 if there were no valid moves, but depth > 0 so _undo_move was
    //      called, OR there was a valid move so it was made, but it didn't
    //      result in a win.
    //    1 if game is won (there was a valid move, it was made, and the
    //      game was won)
    //    2 if game is lost (no valid moves, and depth == 0)
    //    3 if self.n_states_checked > self._max_states and
    //      self._max_states>0
    //-------------------------------------------------------------------------
    int PerformMove();

    //--------------------------------------------------------------------------
    // Undo last move.
    //
    // Reverse the three-step process described in PerformMove(), except
    // Step (2) is not reversed as the counters that were updated are meant
    // to be cumulative, and reversal of Step (3) simply involves popping the
    // top elements from the relevant stacks.
    //--------------------------------------------------------------------------
    void UndoMove(const bool &no_print);
    //void InitializeDeck();
    void PrintDeck() const;
    void PrintLastMoveInfo() const;
    // Print moves_left_ attribute to stderr every 5 million states
    void SummarizeState() const;
}; // end class Agnes

class FileNotFoundError : public std::runtime_error {
  public :
    FileNotFoundError(const std::string & file_name="")
       : runtime_error("File not found: " + file_name),
             filename_(file_name) {};

    std::string filename_;
    ~FileNotFoundError() throw() {};
};

} // namespace quagnes

#endif

