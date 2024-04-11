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
// File: AgnesState.h
// Date: 2019
// Author: Ray Griner
// Purpose: Header file for AgnesState.cpp
// Changes:
// [20240407] Make depth_ and n_stock_left_ int instead of unsigned variables.
//   as it is possible for depth to overflow a uint16. For n_stock_left, there
//   is likely nothing to be gained by using a uint8_t.
//------------------------------------------------------------------------------

#ifndef _QUAGNES_AGNESSTATE_H
#define _QUAGNES_AGNESSTATE_H

#include <string>
#include <vector>
#include <set>
#include <stack>
#include <array>

namespace quagnes {

#define N_SUIT 4
#define N_RANK 13
#define N_TO_TRACK 100
#define N_PILE 7
#define N_CARD 52

// A playing card.
struct Card {
  int rank; // valid values: 0-12
  // suit valid values: 0-3. ({0,2} and {1,3} are defined to be the same color)
  int suit;
  Card() : rank(0), suit(0) {}
  Card(int rank, int suit) : rank(rank), suit(suit) {}
};

// 1:1 mapping between string in `Agnes.move_to_empty_pile_` and these values
enum class EmptyRule {None, Any1, AnyRun, High1, HighRun};

// Define the three types of moves (four if you count None)
enum class Moves {None, InTableau, ToFoundation, Deal};

// Indicate whether a tableau or foundation move split a same-suit sequence
// or joined a same-suit sequence (TablMove) only or neither. Used to update
// AgnesState.in_suit_seq_ when the move is done or undone.
enum class TablType {None, Split, Join};

// A move in the game
struct Move {
  Moves movetype;  // InTableau, ToFoundation, Deal, None
  uint8_t from;  // Pile moved from (ToFoundation, TablMove)
  uint8_t suit;  // Suit moved (ToFoundation)
  uint8_t n_cards;  // n cards moved (TablMove)
  uint8_t to;    // Pile moved to (TablMove)
  bool expose;   // Did move expose a new card? (ToFoundation, TablMove)
  TablType tabltype; // See TablType enum above (ToFoundation, TablMove)

  Move(): movetype(Moves::None), from(0), suit(0), n_cards(0), to(0),
     expose(false), tabltype(TablType::None) {}
  Move(Moves movetype, int from, int suit, int n_cards, int to, bool expose,
     TablType tabltype) : movetype(movetype), from(from), suit(suit),
     n_cards(n_cards), to(to), expose(expose), tabltype(tabltype) {}
};

// Group the options passed to AgnesState::set_n_movable
struct SetNMovableOpts {
  bool move_same_suit;
  bool split_runs;
  bool split_empty_stock;
  SetNMovableOpts(bool move_same_suit, bool split_runs,
          bool split_empty_stock) :
    move_same_suit(move_same_suit),
    split_runs(split_runs),
    split_empty_stock(split_empty_stock) {}
};

// Deck of cards
typedef std::array<Card, N_CARD> Deck;

//------------------------------------------------------------------------------
// Store information about the last tableau move done for a pile
//
// Attributes
// ----------
// depth : int
//     Depth of the last state when a tableau move was done to or from this
//     pile. Reset to 0 when the pile is dealt on or has a card put to
//     foundation.
// n_moved : int
//     If depth != 0, stores the number of cards moved.
// moved_to : bool
//     If depth != 0, stores whether this was the pile moved to (vs moved from)
// can_move_to_found : bool
//     Set to false if depth is not 0 and the bottom card in the run
//     being moved could have been put into the foundation before the
//     move. This is set on the pile that was moved to. Otherwise, true
//------------------------------------------------------------------------------
struct LastMoveInfo {
  int depth = 0;
  int n_moved = 0;
  bool moved_to = false;
  bool can_move_to_found = true;
};

//------------------------------------------------------------------------------
// Represents a tableau pile.
//
// Attributes
// ----------
// n_movable : list[int]
//     List of ints representing the number of cards that can be moved from
//     the tableau column (without considering whether there is a
//     destination that columns can be moved to). For example, if the
//     bottom cards in a column are 3S, 4S, 5H, the value is 2, even if
//     there is no 5C or 5S exposed.
// exposed : list[Card]
//     List of exposed cards in the pile. Highest index indicates cards
//     at the bottom of the column in the tableau.
// hidden: list[Card]
//     List of hidden cards in the pile. Highest index indicates cards
//     at the bottom of the column in the tableau.
//------------------------------------------------------------------------------
struct AgnesPile {
  std::vector<int> n_movable = {};
  std::vector<Card> hidden = {};
  std::vector<Card> exposed = {};
};

typedef std::array<quagnes::AgnesPile, 7>::size_type PileSizeType;
//-----------------------------------------------------------------
// Class declarations
//-----------------------------------------------------------------
class AgnesState {
  public:
    AgnesState();
    void Print();
    // Compressed string representation of the state.
    // Stored in `Agnes.losing_states` attribute.
    void UpdateHash();
    // Uncompressed string representation of the state used in Print().
    std::string ToUncompStr();
    AgnesState(const AgnesState& other) = default;
    AgnesState& operator=(const AgnesState& other) = default;

    // getters
    int depth();
    uint8_t n_stock_left();
    //Move curr_move();
    std::vector<Move> valid_moves();
    bool is_loop();
    bool is_loser();
    std::string hash();

    // simple setters
    void ClearValidMoves();
    void set_is_loop(bool value);
    void set_is_loser(bool value);
    void set_curr_move(const Move &);

    // other

    // Set piles_[pile_index].n_movable.
    //
    // Note this just checks the number of cards from the bottom of the
    // pile than can be moved according to the rules. It does not check
    // whether there is somewhere the selected set of cards can be moved
    // to. The latter is done in `set_valid_moves`.
    //
    // The value of n_stock_left_ is used in addition to the
    // parameters passed in to determine if the stock is empty. Therefore,
    // this function must be called for all piles when the stock is
    // made empty (or such a move undone).
    //
    // Arguments
    // ---------
    // pile_index : int
    //     Index of the pile to check
    // snm_opts : SetNMovableOpts
    //     Tuple holding `Agnes` parameter values `move_same_suit`,
    //     `split_runs`, and `split_empty_stock`.
    //
    // Returns
    // -------
    // List of integers with number of cards that can be moved from a
    // pile.
    //--------------------------------------------------------------------------
    void set_n_movable(const int pile_index, const SetNMovableOpts& snm_opts);

    //  Set valid_moves_ to the valid moves available.
    //
    //  Three types: DealMove()
    //               TablMove(from_=, to_=, n_cards=, expose=, tabltype=)
    //               MoveToFound(from_=, suit=, expose=, tabltype=)
    //
    //  The `n_movable` attribute in a pile contains a list of the number
    //  of cards in a pile that can be moved (based on the `move_same_suit`
    //  and `split_runs` parameters when the `Agnes` object was created.
    //  It also takes into account the optimization that there is no
    //  benefit to splitting a sequence between two cards of the same
    //  suit when the stock is empty, unless the empty rule is 'any 1' or
    //  'high 1'.
    //
    //  This function creates a list of all valid moves that can be done
    //  by seeing if a deal is available, which cards can be moved to which
    //  target piles, and which cards can be moved to the foundation.
    //
    //  Various optimizations reduce the moves available. Forcing a move
    //  means this is the only move that can be played. (If multiple moves
    //  meet the forcing criteria, the last forced is used.)
    //
    //  (1) If n_stock_left == 2 and if track_threshold > 2, do not allow
    //  the deal if there was a move between two piles that won't be covered
    //  by the final deal. (Purpose is to reduce the search tree by forcing
    //  these moves to occur after the final deal. This is only done when
    //  track_threshold > 2 because otherwise we need to store the
    //  information from LastMoveInfo in the `Agnes.losing_states` set.
    //
    //  (2) If there are multiple empty piles that won't be covered by
    //  a future deal, only allow moves to the first pile.
    //
    //  (3) If we are not using the `losing_states` set (track_threshold >
    //  `n_stock_left`), do not reverse a move (ie, move the same number
    //  of cards from one pile to another).
    //
    //  (4) Force joins by suit sequence when the stock is empty and
    //  `split_empty_stock is false` if the same-color top card of the
    //  pile being moved satisfies (i) next lowest card is already in
    //  the foundation or (ii) `move_same_suit = false` and already under
    //  the next-highest card of the same suit or (iii)
    //  `move_same_suit = false` and next-highest card is available to be
    //  played on. For example, suppose we have a run starting with 4C
    //  that we might put under 5C. This move is forced if (i) 3S in
    //  foundation, (ii) `move_same_suit = false` and 4S already under 5S,
    //  or (iii) `move_same_suit = false` and 5S is last in pile.
    //
    //  (5) Force move to foundation if (a) rank <= (highest rank in
    //  foundation of same-color suit + 2) or (b) stock is empty and
    //  `split_empty_stock = false` and next lowest rank of the same
    //  color is already under in sequence under the same suit.
    //
    //  (6) Additional restriction on splitting same suit besides those
    //  implied by `split_runs` (which were implemented in set_n_movable).
    //  Only allow splits of same suit if source or target will be covered
    //  by future deal, or split_empty_stock=true or
    //  Card(src_card.rank, same_color_suit(src_card)) is not in the foundation,
    //  where src_card is the highest-rank card in the part of the pile that
    //  is being moved.
    //
    //  Parameters
    //  ----------
    //  enum_to_empty_pile : EmptyRule
    //      `Agnes.move_to_empty_pile` parameter converted to enum.
    //  move_same_suit : bool
    //      `Agnes.move_same_suit` parameter
    //  split_empty_stock : bool
    //      `Agnes.split_empty_stock` attribute
    //  track_threshold : int
    //      `Agnes.track_threshold` attribute
    //  last_move_info : std::array<LastMoveInfo, N_PILE>&
    //      The LastMoveInfo information for this state
    //--------------------------------------------------------------------------
    void set_valid_moves(EmptyRule move_to_empty_pile,
      const bool move_same_suit,
        const bool split_empty_stock,
        const int track_threshold,
        const std::array<LastMoveInfo, N_PILE>& last_move_info);

    // Simpler logic than the above. We (redundantly) store the valid moves
    //  in valid_moves_ and the Agnes.all_valid_moves stack. When a move
    //  is undone, the stack is popped and then this function is called to
    //  update valid_moves_.
    //-------------------------------------------------------------------------
    void set_valid_moves(const std::vector<Move> &);

    //-------------------------------------------------------------------------
    // Deal a card from the stock onto the pile with index pile_index.
    //
    //  Set `last_in_pile_` = true for the card being dealt and set
    //  `last_in_pile_` = false for the card now covered by the deal (if
    //  one exists). Set `in_suit_seq_` = true for the dealt card if the card
    //  happened to be dealt in sequence under a card of the same suit
    //  in the exposed pile.  Card dealt is appended to the end of the
    //  `exposed` pile if `face_up_ = true` and the `hidden` pile otherwise.
    //  Decrease `n_stock_left_` by 1.
    //
    //  Parameters
    //  ----------
    //  pile_index : int
    //      Pile being dealt to.
    //  deck : Deck
    //      Deck from which we are dealing.
    //  face_up : bool
    //      Indicates whether card is dealt face-up (and put in the
    //      piles_[pile_index].exposed pile) or face-down (and put
    //      in
    //-------------------------------------------------------------------------
    void DealOntoPile(int pile_index, const Deck &deck, bool face_up);

    //-------------------------------------------------------------------------
    //  Undo deal of one card for pile identified by pile_index.
    //
    //  Set `in_suit_seq` and `last_in_pile` = false for the card being
    //  undealt. If there is a card above this that will be now last,
    //  set `last_in_pile` to true for that card. Pop the last card
    //  from `piles[pile_index].exposed` and increase `n_stock_left` by 1.
    //
    //  Unlike deal_onto_pile, we don't need to handle face-down deals
    //  because we never undo them.
    //
    //  Attributes
    //  ----------
    //  pile_index : int
    //      Index of the pile for which the deal is undone
    //--------------------------------------------------------------------------
    void UndoDealForPile(int pile_index);

    // Play the base card into the foundation.
    void PlayBaseCard(const Card &card);

    //--------------------------------------------------------------------------
    //  Check if game is unwinnable after a deal.
    //
    //  This should only be called if `Agnes.move_to_empty_pile_ == "none"`.
    //  Otherwise, the game might be winnable even though suits are
    //  blocked because the 'kings' can be moved to an empty pile to
    //  unblock the cards beneath them. (Here we use 'king' to refer to
    //  the highest-rank card.)
    //
    //  Creates a graph indicating whether a given 'king' is covering in
    //  a pile a lower rank card of the same suit or a 'king' of another
    //  suit. If the graph has a cycle, the game cannot be won.
    //
    //  Returns
    //  -------
    //  bool that is true if any pile or combination of piles blocks a win.
    //--------------------------------------------------------------------------
    bool IsAnyPileBlocked();

    //--------------------------------------------------------------------------
    // Functions to implement and undo the three different types of moves.
    // These all update the piles_, foundation_, in_suit_seq_, last_in_pile_,
    // n_stock_left_ as appropriate and increment or decrement depth_.
    //
    // After a move is done, set_n_movable() is called on any piles touched
    // (or for the last deal, on all piles, since we eliminate some moves from
    // consideration when the stock is empty, see `set_valid_moves()` for
    // details).
    //
    // Additional information about the move is stored in the last_move_info
    // array.
    //--------------------------------------------------------------------------
    void UndoMoveToFoundation(const Move & curr_move,
                              const SetNMovableOpts & snm_opts);
    void MoveToFoundation(const Move & curr_move,
                          std::array<LastMoveInfo, N_PILE>& last_move_info,
                          const SetNMovableOpts & snm_opts) ;
    void DealMove(const Deck &deck,
                  std::array<LastMoveInfo, N_PILE>& last_move_info,
                  const SetNMovableOpts & snm_opts) ;
    void UndoDeal(const SetNMovableOpts & snm_opts);
    void TableauMove(const Move & curr_move,
                     std::array<LastMoveInfo, N_PILE>& last_move_info,
                     const SetNMovableOpts & snm_opts) ;
    void UndoTableauMove(const Move & curr_move,
                         const SetNMovableOpts & snm_opts) ;
  private:
    int depth_;
    uint8_t n_stock_left_;
    // the 7 piles in the game
    std::array<AgnesPile, N_PILE> piles_;
    Move curr_move_;
    std::array<int, N_SUIT> foundation_;
    // True if a card is in an exposed pile in sequence under a card of
    // the same suit or in the foundation. This is important because once the
    // stock is empty, then most of the time (ie, when
    // `Agnes.split_empty_stock = false`), there is no benefit to splitting a
    // movable sequence at this point for a move. Can also be used to determine
    // when a move that joins two sequences can be forced (see set_valid_moves
    // for details).
    std::array<std::array<bool, N_SUIT>, N_RANK> in_suit_seq_;
    // True if a card is the last one in an exposed pile or in the foundation.
    // See `set_valid_moves()` for why this is useful.
    std::array<std::array<bool, N_SUIT>, N_RANK> last_in_pile_;
    // Valid moves remaining at this state.
    std::vector<Move> valid_moves_;
    // stores whether a state has been identified as a loop
    bool is_loop_;
    bool is_loser_;
    std::string hash_;
    //std::array<int, N_PILE> sort_order_;

    //void set_sort_order(EmptyRule move_to_empty_pile);
    void PrintTableau();
    void PrintInSuitSeq();
    void PrintValidMoves(const std::vector<Move> &valid_moves);
    void PrintFoundation();
};

} // namespace quagnes

#endif

