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
// [20240411RG] (1) Add lower_last_, upper_last_, last_same_color_not_suit
//   attributes to AgnesClass to support checking if we can force the last deal
//   by seeing if the last couple cards can be put immediately into the
//   foundation. Also added SetLastCards function to set these and IsDealForced
//   function to check if deal can be forced. (2) Add InFoundation and
//   same_color_suit functions.
// [20240416RG] (1) Switch C-style #define constants to const variables.
//   (2) Mark a few other functions as const. (3) Remove split_empty_stock
//   parameter from IsDealForced as we have simplified the logic so that we
//   no longer force deal if !split_empty_stock and the 1-lower next suit is
//   in-suit sequence.
// [20240420] (1a) Change hash_ attribute to compstr_ and UpdateHash to
//   UpdateCompStr since there is no information loss when we compress.
//   (1b) Build compstr_ using push_back() instead of ostream; (1c) pass
//   face_up parameter to UpdateCompStr so that we don't waste memory on the
//   pile-markers of the hidden piles when we know they are empty.
// [20240421] Add `CalculateMaxPossibleScore` declaration.
// [20240423] Made Card a class. Added `value` attribute and IsSameSuit()
//   and IsSameColor() functions that take a reference to another card as
//   parameter. The value of the card is now represented in a single uint8_t
//   instead of an int for rank and suit.
// [20240428] (1) Add ArraySix class and hash function. (2) Create new typedef
//   `StateForSet` for the `compstr_` attribute. These objects are also stored
//   in the `check_loops` and `losing_states` sets in `Agnes` objects. Set the
//   typedef to `ArraySix` for now. (3) Correct year in change log.
//------------------------------------------------------------------------------

#ifndef _QUAGNES_AGNESSTATE_H
#define _QUAGNES_AGNESSTATE_H

#include <string>
#include <cstring>
#include <vector>
#include <set>
#include <stack>
#include <bitset>
#include <array>
#include <cstdint>

namespace quagnes {

const int kNSuit = 4;
const int kNRank = 13;
const int kNToTrack = 100;
const int kNPile = 7;
const int kNCard = 52;
const int kNSymbol = 64;

// A playing card.
class Card {

  public:
    Card() : value_(99) {}
    Card(int rank, int suit) : value_(suit*16+rank) {}

    void set_value(int rank, int suit);
    bool IsSameSuit(Card & other) const;
    bool IsSameColor(Card & other) const;
    bool IsSameColor(const Card & other) const;
    // rank is 0-13
    int rank() const;
    // suit is 0-4, where {0,2} and {1,3} are the same color
    int suit() const;
    uint8_t value() const;
    // return the other suit of the same color as an int
    // return (((value_ >> 4) + 2) % 4) is faster??
    int same_color_suit() const;
  private:
    uint8_t value_;
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

// An array of size 64, where each value is at most 6 bits
class ArraySix {
  public:
    ArraySix() : bits_() {}
    void reset();
    uint8_t get_pos(size_t pos) const;
    void set_pos(const size_t pos, const uint8_t value);
    bool operator<(const ArraySix& right) const;
    bool operator>(const ArraySix& right) const;
    bool operator==(const ArraySix& right) const;
    void Print() const;
    const uint64_t *data() const { return bits_.data(); }

  private:
    std::array<uint64_t, 6> bits_;
};

//------------------------------------------------------------------------------
// Implement the hash function `_Hash_bytes` from gcc v12.2.0 (with length
// hard-coded to 48 bytes and code removed that handles cases when `len` is not
// divisible by 8.
//------------------------------------------------------------------------------
struct ArraySix_hash {
  inline std::size_t shift_mix(std::size_t v) const {
    return v ^ (v >> 47);
  }

  inline std::size_t unaligned_load(const char* p) const {
    std::size_t result;
    memcpy(&result, p, sizeof(result));
    return result;
  }

  std::size_t operator()(const ArraySix& x) const {
    static const std::size_t len = 48UL; // length is hard-coded to 48 bytes
    static const std::size_t seed = static_cast<std::size_t>(0xc70f6907UL);
    static const std::size_t mul = (((std::size_t) 0xc6a4a793UL) << 32UL)
                              + (std::size_t) 0x5bd1e995UL;
    const char* const buf = reinterpret_cast<const char*>(x.data());

    std::size_t hash = seed ^ (len * mul);
    for (const char* p = buf; p != buf + len; p += 8) {
        const std::size_t data = shift_mix(unaligned_load(p) * mul) * mul;
        hash ^= data;
        hash *= mul;
    }
    hash = shift_mix(hash) * mul;
    hash = shift_mix(hash);
    return hash;
  }
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
typedef std::array<Card, kNCard> Deck;
// Representation of the tableau state that will be stored in the sets that
// track losing states and look for loops.
typedef std::vector<char> StateForSet;

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
    void Print() const;
    // Compressed string representation of the state.
    // Stored in `Agnes.losing_states` attribute.
    void UpdateCompStr(bool face_up, EmptyRule enum_to_empty_pile);
    void PushToBitset(int &pos, uint8_t value);
    void UpdateSymbolCounts(bool face_up, EmptyRule enum_to_empty_pile);
    // Uncompressed string representation of the state used in Print().
    std::string ToUncompStr() const;
    AgnesState(const AgnesState& other) = default;
    AgnesState& operator=(const AgnesState& other) = default;
    // Set information about the two last cards in the deck that
    // will be used by the IsDealForced function
    void SetLastCards(const std::array<Card, kNCard> & deck);
    // Inline to return whether a card is in the foundation. Pass rank and suit
    // separately. Otherwise, we might have to create a Card just to call this,
    // which would be otherwise unnecessary.
    bool InFoundation(int rank, int suit) const {
      return (foundation_[suit] >= rank);
    };

    // getters
    int depth() const;
    uint8_t n_stock_left() const;
    //Move curr_move();
    std::vector<Move> valid_moves() const;
    bool is_loop() const;
    bool is_loser() const;
    StateForSet compstr() const;
    uint64_t cum_length() const;
    std::array<uint64_t, kNSymbol> cum_symbol_count0() const;
    std::array<uint64_t, kNSymbol> cum_symbol_count1() const;

    // simple setters
    void ClearValidMoves();
    void set_is_loop(bool value);
    void set_is_loser(bool value);
    void set_curr_move(const Move &);

    // Calculate the maximum possible score from the initial tableau layout
    //
    // 1. If enum_to_empty_pile != EmptyRule::None, return 52, otherwise...
    // 2. Make a graph showing which suits are blocked by which kings in the
    //    tableau.
    // 3. Make a list of the suits corresponding to vertices that are in cycles
    //    in the graph. Kings of these suits can never be put into the
    //    foundation.
    // 4. Make min_blocked[suit]: the lowest-rank card blocked by such kings.
    //    If no cards are blocked, min_blocked[suit] = 13.
    // 5. return max_possible_score = 52 - sum(13 - min_blocked[suit])
    //
    // Arguments
    // ---------
    // enum_to_empty_pile : EmptyRule
    //     `Agnes.enum_to_empty_pile`
    //
    // Returns
    // -------
    // Integer as described above
    //-------------------------------------------------------------------------
    int CalculateMaxPossibleScore(const EmptyRule & enum_to_empty_pile);

    // Set piles_[pile_index].n_movable
    //
    //  Note this just checks the number of cards from the bottom of the
    //  pile than can be moved according to the rules. It does not check
    //  whether there is somewhere the selected set of cards can be moved
    //  to. The latter is done in `set_valid_moves`.
    //
    //  It sets the number cards from the bottom of the pile that can be
    //  moved according to the `move_same_suit` and `split_runs` parameter.
    //
    //  While doing so, it also applies additional optimizations
    //  when the stock is empty and `split_empty_stock` is false about
    //  whether to allow splitting a run between two cards of the same
    //  suit (even when `split_runs` is True). Namely:
    //    (1) If runs are moved by color, such splits are never allowed.
    //    (2) If runs are moved by suit, such splits are not allowed
    //    if it is certain the last card in the pile being moved is not
    //    needed as a move target for the suit of the same color (ie,
    //    if Card(last_card.rank - 1, last_card.same_color_suit) is
    //    in the foundation or can be forced to the foundation, the split is
    //    not allowed. [However, this second condition is implemented in
    //    set_valid_moves rather than here, because this function is only
    //    called when piles are touched and it's possible an operation on a
    //    different pile activated or deactivated condition (2).
    //
    //  Because the function checks whether `self.n_stock_left == 0`, it
    //  must be called for all piles when the stock is made empty (or such
    //  a move undone). The call must follow updating of all piles.
    //
    // Arguments
    // ---------
    // pile_index : int
    //     Index of the pile to check
    // snm_opts : SetNMovableOpts
    //     Tuple holding `Agnes` parameter values `move_same_suit`,
    //     `split_runs`, and `split_empty_stock`.
    //--------------------------------------------------------------------------
    void set_n_movable(const int pile_index, const SetNMovableOpts& snm_opts);

    // Set self.valid_moves to the valid moves available.
    //
    //  Three types: DealMove()
    //               TablMove(from_=, to_=, n_cards=, expose=, tabltype=)
    //               MoveToFound(from_=, suit=, expose=, tabltype=)
    //
    //  This function creates a list of all valid moves that can be done
    //  by seeing if a deal is available, which cards can be moved to which
    //  target piles, and which cards can be moved to the foundation.
    //
    //  Various optimizations reduce the moves available. Forcing a move
    //  means this is the only move that can be played. Forced moves are
    //  designed to reduce the space of moves that need to be searched
    //  without changing the final score of the game. If more than one
    //  move meets the criteria to be forced, the last move is chosen.
    //
    //  (1) If there are multiple empty piles that won't be covered by
    //  a future deal, only allow moves to the first pile.
    //
    //  (2) If we are not using the `losing_states` set (track_threshold >
    //  `n_stock_left`), do not reverse a move (ie, move the same number
    //  of cards from one pile to another).
    //
    //  (3) Force joins by suit sequence when the stock is empty and
    //  `split_empty_stock is False` if the same-color top card of the
    //  pile being moved satisfies (i) next lowest card is already in
    //  the foundation or (ii) `move_same_suit = False` and already under
    //  the next-highest card of the same suit or (iii)
    //  `move_same_suit = False` and next-highest card will be available
    //  to be played on after the move (last_in_pile or will be the new
    //  last card after the move from the current pile). For example,
    //  suppose we have a run starting with 4C that we might put under 5C.
    //  This move is forced if (i) 3S in foundation, or
    //  (ii) `move_same_suit = False` and 4S already under 5S, or
    //  (iii) `move_same_suit = False` and 5S is last in pile or we are
    //  moving the 4C from under the 5S.
    //
    //  (4) Force move to foundation if (a) Card(rank - 2, same_color_suit)
    //  is already in the foundation or (b) stock is empty and
    //  `split_empty_stock = False` and Card(rank - 1, same_color_suit)
    //  is already under Card(rank, same_color_suit).
    //
    //  (5) Force the last deal if the two dealt cards can immediately be
    //  forced to the foundation.
    //
    //  (6) There are additional restrictions when the stock is empty for
    //  when a run can be split between two cards of the same suit, even
    //  when `split_runs=True`. When `move_same_suit is False`, the split
    //  is never allowed (enforced in the `set_n_movable` function). Otherwise,
    //  the split is allowed only if the last card in the pile being moved
    //  will never need to have the same-color suit added, which means
    //  Card(rank - 1, same_color_suit) is in the foundation or can be forced
    //  to the foundation [Card(rank - 2, same_color_suit in foundation), and
    //  Card(rank - 3, suit) in foundation] (where all ranks and suits refer to
    //  the last card of the pile being moved).
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
    //  last_move_info : std::array<LastMoveInfo, kNPile>&
    //      The LastMoveInfo information for this state
    //--------------------------------------------------------------------------
    void set_valid_moves(EmptyRule move_to_empty_pile,
      const bool move_same_suit,
        const bool split_empty_stock,
        const int track_threshold,
        const std::array<LastMoveInfo, kNPile>& last_move_info);

    // Check if deal can be forced (if cards can be forced to foundation).
    //
    //  Generally, a card can be forced to the foundation if (1) the card
    //  below it is already in the foundation, and
    //  (2a) Card(rank - 2, same_color_suit) is already in the foundation, or
    //  (2b) Card(rank - 1, same_color_suit) is in sequence under a card of
    //  the same suit and split_empty_stock is False.
    //
    //  Here we consider only (1)+(2a), as the correctness is easier to
    //  discern and the effect of (1)+(2b) is negligible.
    //
    //  This code also handles the case where the two last cards might be
    //  the same color but different suits (in which case putting the first
    //  card into the foundation will increase the threshold at which the
    //  second card would be forced into the foundation).
    //
    //  Arguments
    //  ---------
    //
    //  Returns
    //  -------
    //  bool indicating whether the deal should be forced.
    //-------------------------------------------------------------------------
    bool IsDealForced() const;

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
    bool IsAnyPileBlocked() const;

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
                              const SetNMovableOpts & snm_opts,
                              const EmptyRule & move_to_empty_pile);
    void MoveToFoundation(const Move & curr_move,
                          std::array<LastMoveInfo, kNPile>& last_move_info,
                          const SetNMovableOpts & snm_opts,
                          const EmptyRule & move_to_empty_pile) ;
    void DealMove(const Deck &deck,
                  std::array<LastMoveInfo, kNPile>& last_move_info,
                  const SetNMovableOpts & snm_opts,
                  const EmptyRule & move_to_empty_pile) ;
    void UndoDeal(const SetNMovableOpts & snm_opts,
                  const EmptyRule & move_to_empty_pile);
    void TableauMove(const Move & curr_move,
                     std::array<LastMoveInfo, kNPile>& last_move_info,
                     const SetNMovableOpts & snm_opts,
                     const EmptyRule & move_to_empty_pile) ;
    void UndoTableauMove(const Move & curr_move,
                         const SetNMovableOpts & snm_opts,
                         const EmptyRule & move_to_empty_pile) ;
  private:
    int depth_;
    uint8_t n_stock_left_;
    // the 7 piles in the game
    std::array<AgnesPile, kNPile> piles_;
    Move curr_move_;
    std::array<int, kNSuit> foundation_;
    // True if a card is in an exposed pile in sequence under a card of
    // the same suit or in the foundation. This is important because once the
    // stock is empty, then most of the time (ie, when
    // `Agnes.split_empty_stock = false`), there is no benefit to splitting a
    // movable sequence at this point for a move. Can also be used to determine
    // when a move that joins two sequences can be forced (see set_valid_moves
    // for details).
    std::array<std::array<bool, kNSuit>, kNRank> in_suit_seq_;
    // True if a card is the last one in an exposed pile or in the foundation.
    // See `set_valid_moves()` for why this is useful.
    std::array<std::array<bool, kNSuit>, kNRank> last_in_pile_;
    // Valid moves remaining at this state.
    std::vector<Move> valid_moves_;
    // stores whether a state has been identified as a loop
    bool is_loop_;
    bool is_loser_;
    StateForSet compstr_;
    // 20240413
    // Next four variables have information used in the is_deal_forced column
    // it seems likely more efficient to calculate once and retrieve rather
    // than recalcuate each time the function is called.
    // lower-ranked of the last two cards in the deck
    Card lower_last_;
    // higher-ranked of the last two cards in the deck
    Card upper_last_;
    // true if lower_last and upper_last are the same suit and in order
    bool last_same_suit_seq_;
    // true if lower_last and upper_last are the same color and not the same
    // suit
    bool last_same_color_not_suit_;
    // indicates listing which piles
    std::array<int, kNPile> sort_order_;
    // Sum of lengths stored in losing-states set.
    uint64_t cum_length_;
    // Frequency each symbol is stored in losing-states set. Symbols are:
    // each of the 52 cards, an end-of-pile marker, and an end-of-tableau marker
    // (since sometimes we can avoid writing all 7 piles). The count1_ variable
    // adds two symbols that indicate whether a card is in sequence under a
    // card of the same suit or color. However, the kNSymbol is set a little
    // larger so that the card value can be used as the index.
    std::array<uint64_t, kNSymbol> cum_symbol_count0_;
    std::array<uint64_t, kNSymbol> cum_symbol_count1_;

    void set_sort_order(EmptyRule move_to_empty_pile);
    void PrintTableau() const;
    void PrintInSuitSeq() const;
    void PrintValidMoves(const std::vector<Move> &valid_moves) const;
    void PrintFoundation() const;
    void InitBlockGraph(std::array<std::array<bool, kNSuit>,
                        kNSuit>& graph) const;
};

} // namespace quagnes

#endif

