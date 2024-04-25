//  A program for solving Agnes solitaire
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
// File: AgnesState.cpp
// Date: 2019
// Author: Ray Griner
// Purpose: AgnesState class and related functions
// Changes:
// [20240413RG] (1) Add IsDealForced to force deals when stock has two cards
//   left and the cards we are waiting for can immediately be placed to the
//   foundation. Also SetLastCards function to set values of lower_last and
//   upper_last which at least for now needs to be done after deck
//   initialization. (2) The last_in_pile variable doesn't consider that the
//   next higher card of the same-color suit might be above the pile we are
//   moving (in which case we should force it just like for last_in_pile).
//   Add src_is_next_suit_seq flag to handle this case. (3) Bug fix:
//   set_n_movable. When move_same_suit, we should only allow split if the last
//   card in the pile being moved isn't needed to hold the next suit (so we
//   check whether Card(last_card.rank-1, next_suit) is in the foundation or
//   can be forced to foundation.
//   (4) set_valid_moves: remove logic that did not allow splitting same-suit
//   when the empty piles would not be covered by a future deal. This is
//   incorrect when `move_same_suit = True` and already implemented in
//   `set_n_movable` when `move_same_suit = False`.
//   (5) Sort piles that cannot be covered by a future deal. For example, if
//   the stock is empty, and we switch what is in piles (3) and (4), the same
//   score must be obtained.
//   (6) Add assert statements to beginning of the 6 functions that do moves
//   and unmoves.
// [20230416] (1) Change C-style define constants to const
//   (2) Mark a few other functions as const. (3) Remove split_empty_stock
//   parameter from IsDealForced and simplify the logic so that we no longer
//   force deal if !split_empty_stock and the 1-lower next suit is in-suit
//   sequence.
// [20230420] (1a) Change hash_ attribute to compstr_ and UpdateHash to
//   UpdateCompStr since there is no information loss when we compress.
//   (1b) Build compstr_ using push_back() instead of ostream; (1c) pass
//   face_up parameter to UpdateCompStr so that we don't waste memory on the
//   pile-markers of the hidden piles when we know they are empty.
//   (2a) Change pile_less so that empty piles are always at the end.
//   (2b) UpdateCompStr: use highest bit of byte to indicate a card starts a
//   pile. When an empty pile is found and we know all later piles must be
//   empty (ie, n_stock_left == 0 or (n_stock_left == 2 and pile_index > 1),
//   no longer write markers for empty piles.
// [20230423] Made Card a class. Move function definitions to this file.
// [20230424] (1) Bug fixes for when face_up is false (a) last_in_pile was not
//   being set to true when moving last card from exposed pile with non-empty
//   hidden pile; (b) `set_valid_moves` when moving to empty pile, moving all
//   exposed cards should always be allowed when hidden pile is not empty
//   (2) Creation of `compstr`. (a) write string for hidden piles like we do
//   for exposed piles, but only add 1 to the card value instead of 48 so
//   that 1+value <= 63 and we can use the two highest bits to indicate the
//   first card in exposed and hidden piles, respectively (b) In cases when an
//   empty exposed pile was not the last empty exposed pile, the byte indicating
//   an empty pile was written twice.
//------------------------------------------------------------------------------

#include <iostream>
#include <sstream>
#include <fstream>
#include <algorithm>
#include <cassert>

#include "AgnesState.h"

using std::array;
using std::cout;
using std::string;

namespace quagnes {

//------------------------------------------------------------------------------
// STATIC FUNCTION PROTOTYPES
//------------------------------------------------------------------------------
static bool visit(const int vertex,
       const std::array<std::array<bool, kNSuit>, kNSuit>& g,
       std::array<bool, kNSuit>& current_path,
       std::array<bool, kNSuit>& visited);
static void visit_any_cycle(const int vertex,
       const std::array<std::array<bool, kNSuit>, kNSuit>& g,
       std::vector<int>& current_path,
       std::array<int, kNSuit>& visited,
       std::array<bool, kNSuit>& in_any_cycle);
static bool cyclic(const std::array<std::array<bool, kNSuit>, kNSuit>& g);
static bool IsLastDealBlocked(const array<LastMoveInfo,
                              kNPile>& last_move_info);

static string DescribeMove(const Move &move);
bool pile_less(int n_stock_left, int int_left, const AgnesPile &left,
         int int_right, const AgnesPile &right);

//------------------------------------------------------------------------------
// STATIC FUNCTIONS
//------------------------------------------------------------------------------
static string DescribeMove(const Move &move) {
  std::ostringstream retoss;
  int from = static_cast<int>(move.from);
  int n_cards = static_cast<int>(move.n_cards);
  int suit = static_cast<int>(move.suit);
  int to = static_cast<int>(move.to);

  if (move.movetype == Moves::None) {
    retoss << "Initial layout";
  }
  else if (move.movetype == Moves::InTableau) {
    retoss << "Move " << n_cards << " card(s) from pile ";
    retoss << from << " to pile " << to;
    if (move.expose) {
      retoss << " (exposes a card)";
    }
  }
  else if (move.movetype == Moves::ToFoundation) {
    retoss << "Move bottom card from pile " << from;
    retoss << " to foundation " << suit;
    if (move.expose) {
      retoss << " (exposes a card)";
    }
  }
  else if (move.movetype == Moves::Deal) {
    retoss << "Deal";
  }
  return retoss.str();
}

static bool visit(const int vertex,
       const std::array<std::array<bool, kNSuit>, kNSuit>& g,
       std::array<bool, kNSuit>& current_path,
       std::array<bool, kNSuit>& visited) {
  if (visited[vertex]) return false;

  visited[vertex]=true;
  current_path[vertex]=true;

  for (int neighbor=0; neighbor<kNSuit; ++neighbor) {
    if (!g[vertex][neighbor]) continue;   // not a neighbor
    if (current_path[neighbor]
        || visit(neighbor, g, current_path, visited)) {
      return true;
    }
  }
  current_path[vertex]=0;
  return false;
}

// Says whether graph has a cycle
static bool cyclic(const std::array<std::array<bool, kNSuit>, kNSuit>& g) {
  std::array<bool, kNSuit> current_path = {false, false, false, false};
  std::array<bool, kNSuit> visited = {false, false, false, false};
  bool cycle = false;

  for (int v=0; v<kNSuit; ++v) {
    cycle=visit(v, g, current_path, visited);
    if (cycle) return true;
  }
  return cycle;
}

static void visit_any_cycle(const int vertex,
       const std::array<std::array<bool, kNSuit>, kNSuit>& g,
       std::vector<int>& current_path,
       std::array<int, kNSuit>& visited,
       std::array<bool, kNSuit>& in_any_cycle) {
  if (visited[vertex] == 2) return;
  else if (visited[vertex] == 1) {
    assert(!current_path.empty());
    in_any_cycle[vertex] = true;
    for (int i=current_path.size()-1; i>=0; --i) {
      if (current_path[i]==vertex) return;
      else in_any_cycle[current_path[i]] = true;
    }
  } else {
    visited[vertex]=1;
    current_path.push_back(vertex);

    for (int neighbor=0; neighbor<kNSuit; ++neighbor) {
      if (!g[vertex][neighbor]) continue;   // not a neighbor
      visit_any_cycle(neighbor, g, current_path, visited, in_any_cycle);
    }
    visited[vertex] = 2;
    current_path.pop_back();
  }
}

//------------------------------------------------------------------------------
// PUBLIC CLASS FUNCTIONS
//------------------------------------------------------------------------------
AgnesState::AgnesState()
    : depth_(0),
      n_stock_left_(kNCard),
      piles_(),
      curr_move_(),
      foundation_(),
      valid_moves_(),
      //sort_order_(),
      is_loop_(false),
      is_loser_(false),
      compstr_()
{
  for (std::size_t i=0; i<foundation_.size(); ++i) {
    foundation_[i]=-1;
  }
  for (std::size_t i=0; i<sort_order_.size(); ++i) {
    sort_order_[i] = i;
  }
  for (int i=0; i<kNRank; ++i) {
    for (int j=0; j<kNSuit; ++j) {
      in_suit_seq_[i][j] = false;
      last_in_pile_[i][j] = false;
    }
  }
}

void AgnesState::SetLastCards(const std::array<Card, kNCard> & deck) {
  // no need to keep the last two cards in order here and it's better
  if (deck[kNCard-1].rank() < deck[kNCard-2].rank()) {
    lower_last_ = deck[kNCard-1];
    upper_last_ = deck[kNCard-2];
  } else {
    lower_last_ = deck[kNCard-2];
    upper_last_ = deck[kNCard-1];
  }
  last_same_suit_seq_ = (lower_last_.suit() == upper_last_.suit()
      && lower_last_.rank() + 1 == upper_last_.rank());
  last_same_color_not_suit_ = (lower_last_.suit()
                               == upper_last_.same_color_suit());
}

// Getters and simple setters
int AgnesState::depth() const {
  return depth_;
}

uint8_t AgnesState::n_stock_left() const {
  return n_stock_left_;
}

std::vector<Move> AgnesState::valid_moves() const {
  return valid_moves_;
}

bool AgnesState::is_loop() const {
  return is_loop_;
}

bool AgnesState::is_loser() const {
  return is_loser_;
}

string AgnesState::compstr() const {
  return compstr_;
}

// Simple setters
void AgnesState::set_is_loop(bool value) {
  is_loop_ = value;
}

void AgnesState::set_is_loser(bool value) {
  is_loser_ = value;
}

void AgnesState::set_curr_move(const Move &value) {
  curr_move_ = value;
}

void AgnesState::set_valid_moves(const std::vector<Move>& valid_moves) {
  valid_moves_ = valid_moves;
}

void AgnesState::ClearValidMoves() {
  valid_moves_.clear();
}

// Other public functions

void AgnesState::Print() const {
  cout << "\nMove: " << DescribeMove(curr_move_) << "\n\n";
  cout << "piles:" << ToUncompStr() << "\n";
  cout << "depth:" << depth_;
  cout << ", n_stock_left:" << static_cast<int>(n_stock_left_);
  cout << ", valid_moves:";
  PrintValidMoves(valid_moves_);
  cout << "\n";
  PrintFoundation();
  PrintTableau();
  PrintInSuitSeq();
  cout << "\n";
}

string AgnesState::ToUncompStr() const {
  std::ostringstream retoss;

  retoss << std::to_string(n_stock_left_);
  for (const AgnesPile& pile : piles_) {
    retoss << "#";
    for (auto it = pile.exposed.begin(); it != pile.exposed.end(); ++it) {
      if (it != pile.exposed.begin()) { retoss << "-"; }
      retoss << std::to_string((it->suit())*kNRank+(it->rank()));
    }
    retoss << "#";
    for (auto it = pile.hidden.begin(); it != pile.hidden.end(); ++it) {
      if (it != pile.hidden.begin()) { retoss << "-"; }
      retoss << std::to_string((it->suit())*kNRank+(it->rank()));
    }
  }
  return retoss.str();
}

void AgnesState::UpdateCompStr(bool face_up,
                               EmptyRule enum_to_empty_pile) {
  compstr_.clear();
  compstr_.push_back(n_stock_left_);

  //retoss << (n_stock_left_);
  for (PileSizeType pile_index=0; pile_index<kNPile; ++pile_index) {
    //for (const AgnesPile& pile : piles_) {
    AgnesPile& pile = piles_[sort_order_[pile_index]];
    //AgnesPile& pile = piles_[pile_index];
    uint8_t first_card = 0x80;
    for (auto it = pile.exposed.begin(); it != pile.exposed.end(); ++it) {
      // set the first bit to mark the first card in a pile.
      compstr_.push_back(first_card | (1 + it->value()));
      if (first_card) first_card = 0;
    }
    if (first_card) {
      //compstr_.push_back(first_card);
      // piles are sorted so that all empty piles are at the end when they
      // cannot be covered by a future deal. When this occurs, we can break
      // as we know all future piles must be empty.
      if ((!n_stock_left_ || (n_stock_left_ == 2 && pile_index>1))
          && enum_to_empty_pile != EmptyRule::None) {
        break;
      }
      else {
        compstr_.push_back(first_card);
      }
    }
    if (!face_up) {
      first_card = 0x40;
      for (auto it = pile.hidden.begin(); it != pile.hidden.end(); ++it) {
        compstr_.push_back(first_card | (1 + it->value()));
        if (first_card) first_card = 0;
      }
      if (first_card) {
        compstr_.push_back(first_card);
      }
    }
  }
}

void AgnesState::InitBlockGraph(
    std::array<std::array<bool, kNSuit>, kNSuit> &graph) const {
  for (const AgnesPile& pile: piles_) {
    PileSizeType hSize=pile.hidden.size();
    PileSizeType pSize=pile.exposed.size();
    if (hSize+pSize) {
      std::array<bool, kNSuit> king_found = {false, false, false, false};
      // work backwards through the exposed file and then backwards
      // through the hidden pile
      for (PileSizeType j=0; j<pSize; ++j) {
        const Card &card = pile.exposed[pSize-1-j];
        if (card.rank() == 12) {
          king_found[card.suit()] = true;
        }
        for (int k=0; k<kNSuit; ++k) {
          if (king_found[k] && ((card.rank() != 12) || (card.suit() !=k))) {
            graph[k][card.suit()] = true;
          }
        }
      } // end loop over exposed
      for (PileSizeType j=0; j<hSize; ++j) {
        const Card &card = pile.hidden[hSize-1-j];
        if (card.rank() == 12) {
          king_found[card.suit()] = true;
        }
        for (int k=0; k<kNSuit; ++k) {
          if (king_found[k] && ((card.rank() != 12) || (card.suit() !=k))) {
            graph[k][card.suit()] = true;
          }
        }
      } // end loop over piles_.hidden
    }
  }
}

int AgnesState::CalculateMaxPossibleScore(const EmptyRule& enum_to_empty_pile) {
  int max_possible_score = 52;
  if (enum_to_empty_pile != EmptyRule::None) return max_possible_score;

  std::array<bool, kNSuit> in_any_cycle = {false, false, false, false};
  std::array<int, kNSuit> min_rank_blocked = {13, 13, 13, 13};

  {
    std::array<std::array<bool, kNSuit>, kNSuit> graph = {
        false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false};
    InitBlockGraph(graph);

    {
      std::array<int, kNSuit> visited = {0, 0, 0, 0};

      for (int v=0; v<kNSuit; ++v) {
        assert(visited[v] == 0 || visited[v] == 2);
        std::vector<int> current_path = {};
        visit_any_cycle(v, graph, current_path, visited, in_any_cycle);
      }
    }
  }

  for (const AgnesPile& pile: piles_) {
    bool blocked = false;

    if (pile.exposed.empty()) continue;
    for (int card_index = pile.exposed.size() - 1; card_index >= 0;
         --card_index) {
      if (pile.exposed[card_index].rank() == 12
          && in_any_cycle[pile.exposed[card_index].suit()]) blocked = true;
      if (blocked) min_rank_blocked[pile.exposed[card_index].suit()] =
        std::min(min_rank_blocked[pile.exposed[card_index].suit()],
                 pile.exposed[card_index].rank());
    }
    for (int card_index = pile.hidden.size() - 1; card_index >= 0;
         --card_index) {
      if (pile.hidden[card_index].rank() == 12
          && in_any_cycle[pile.hidden[card_index].suit()]) blocked = true;
      if (blocked) min_rank_blocked[pile.hidden[card_index].suit()] =
        std::min(min_rank_blocked[pile.hidden[card_index].suit()],
                 pile.hidden[card_index].rank());
    }
  }

  for (const int min_blocked : min_rank_blocked) {
    max_possible_score -= (13 - min_blocked);
  }
  return max_possible_score;
}

bool AgnesState::IsAnyPileBlocked() const {
  std::array<std::array<bool, kNSuit>, kNSuit> graph = {
      false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false};
  InitBlockGraph(graph);
  return cyclic(graph);
}

void AgnesState::set_n_movable(const int pile_index,
                 const SetNMovableOpts& snm_opts) {
  std::vector<Card> &pile = piles_[pile_index].exposed;
  std::vector<int> &n_movable = piles_[pile_index].n_movable;
  int pile_size = pile.size();
  int above_rank = 99;
  int above_suit = 0;
  n_movable.clear();

  if (pile_size) {
    int max_index = pile_size - 1;
    for (int card_index = max_index; card_index >= 0; --card_index) {
      Card card = pile[card_index];
      if (card_index==0) {
        above_rank = 99;
        above_suit = 0;
      }
      else {
        above_rank = pile[card_index-1].rank();
        above_suit = pile[card_index-1].suit();
      }
      // the next two if statements verify that the card starts a
      // sequence of the same color
      if (!(card.rank() - (max_index - card_index)
          == pile[pile_size-1].rank())) {
        break;
      }

      if (!(card.IsSameSuit(pile[pile_size-1]))) {
        if (snm_opts.move_same_suit
            || !(card.IsSameColor(pile[pile_size-1]))) {
          break;
        }
      }

      bool is_split_same_suit=(above_rank == card.rank() + 1
                   && above_suit == card.suit());
      bool is_split_same_color=(above_rank == card.rank() + 1
                    && above_suit % 2 == card.suit() % 2);

      // Lastly, only add to n_movable if we are allowed to split runs
      // (conditional line 1) or the move is not splitting a run
      // (remaining lines of conditional).
      if (snm_opts.split_runs
        || (snm_opts.move_same_suit && !is_split_same_suit)
        || (!snm_opts.move_same_suit && !is_split_same_color)) {

        // But... we should also never split between two suits in
        // sequence when the stock is 0 and split_empty_stock=false.
        // (1) when we are moving by suit, we can do it as long as we can be
        // sure we won't need to move the other suit onto the pile
        // (but we won't check that here)
        // (2) when moving by color, never need to split runs
        if (!(n_stock_left_)
          && !snm_opts.split_empty_stock
          && ((!snm_opts.move_same_suit && is_split_same_suit))) continue;
        n_movable.push_back( (max_index - card_index) + 1);
      }
    }
  }
}

// Return true if piles 2-6 (using zero-indexing) had a move
static bool IsLastDealBlocked(const array<LastMoveInfo,
                              kNPile>& last_move_info) {
  //return false;
  std::set<int> anydup;
  for (std::size_t i=2; i<kNPile; ++i) {
    if (last_move_info[i].depth) {
      std::pair<std::set<int>::iterator, bool> ret=anydup.insert(
          last_move_info[i].depth);
      if (!ret.second) {
        return true;
      }
    }
  }
  return false;
}

// If there are no cards left in the stock we force moves to the foundation
// if we don't need an exposed card as a target for a move (ie, if the
// card in the same color suit that is one rank lower is already in the
// foundation or in sequence under the same suit). This extends that concept
// to the last deal. If the last deal would result in both cards being
// immediately forced to the foundation, we also force the deal.
bool AgnesState::IsDealForced() const {
  // We already sorted the last two cards so that lower_last_ <= upper_last_
  const Card & card1 = lower_last_;
  const Card & card2 = upper_last_;
  if (n_stock_left_ != 2) return false;
  else {
     bool can_put1 = false;
     bool can_put2 = false;
     // Make sure we can put both the cards in the foundation. Usually it just
     // means checking the (rank -1) card is in the foundation, but we
     // also handle the unlikely case that the last 2 cards are in sequence
     if (last_same_suit_seq_) {
         can_put1 = InFoundation(card1.rank() - 1, card1.suit());
         can_put2 = can_put1;
     }
     else {
         can_put1 = InFoundation(card1.rank() - 1, card1.suit());
         can_put2 = InFoundation(card2.rank() - 1, card2.suit());
     }

     bool card1_forcable = (can_put1
             && (InFoundation(card1.rank() - 2, card1.same_color_suit())));
     bool card2_forcable = (can_put2
             && (InFoundation(card2.rank() - 2, card2.same_color_suit())));
     if (card1_forcable && card2_forcable) {
       return true;
     }
     else if (!last_same_color_not_suit_
              || (!card1_forcable && !card2_forcable)) return false;
     else if (card1_forcable && can_put2
             && (InFoundation(card2.rank() - 3, card1.suit()))) return true;
     else if (card2_forcable && can_put1
             && (InFoundation(card1.rank() - 3, card2.suit()))) return true;
     else return false;
  }
}

void AgnesState::set_valid_moves(EmptyRule move_to_empty_pile,
      const bool move_same_suit,
      const bool split_empty_stock,
      const int track_threshold,
      const array<LastMoveInfo, kNPile>& last_move_info) {
  valid_moves_.clear();
  bool force_move = false;
  Card tgt_card = {0, 0};
  Move forced_move;
  Move reg_move;

  // Deal (MoveDeal move)
  if (n_stock_left_>2
    || (n_stock_left_ == 2
       && (track_threshold <= 2
           || !IsLastDealBlocked(last_move_info)))) {
    reg_move = Move(Moves::Deal, 0, 0, 0, 0, false, TablType::None);
    valid_moves_.push_back( reg_move );
    if (IsDealForced()) {
      force_move = true;
      forced_move = Move(Moves::Deal, 0, 0, 0, 0, false, TablType::None);
    }
  }

  for (PileSizeType pile_index=0; pile_index<piles_.size(); ++pile_index) {
    bool expose = false;
    std::vector<int>& n_movable = piles_[pile_index].n_movable;
    int len_curr_pile = piles_[pile_index].exposed.size();
    if (!len_curr_pile) continue;
    const Card &last_card = piles_[pile_index].exposed[len_curr_pile - 1];

    TablType tabltype = TablType::None;
    for (const int& n_to_move : n_movable) {
      tabltype = TablType::None;
      bool src_in_next_suit_seq = false;
      const Card &src_card = piles_[pile_index].exposed[
          len_curr_pile - n_to_move];
      if (n_to_move == len_curr_pile) {
        expose = piles_[pile_index].hidden.size();
      }
      else {
        expose = false;
        const Card &card_above = piles_[pile_index].exposed[
          len_curr_pile - n_to_move - 1];
        if (src_card.value() + 1 == card_above.value()) {
          tabltype = TablType::Split;
        }
        if (src_card.rank() + 1 == card_above.rank()
            && src_card.IsSameColor(card_above)) {
          src_in_next_suit_seq = true;
        }
      }

      bool found_empty_target = false;
      for (PileSizeType tgt_index=0; tgt_index < piles_.size();
         ++tgt_index) {
        int len_tgt_pile;
        len_tgt_pile = piles_[tgt_index].exposed.size();
        // cannot move a pile onto itself. Furthermore, if there are
        // multiple empty target columns, only the first is considered
        if (tgt_index == pile_index
            || (!len_tgt_pile && found_empty_target)
            || (track_threshold > n_stock_left_
                && (last_move_info[pile_index].depth
                  == last_move_info[tgt_index].depth)
                && last_move_info[pile_index].n_moved == n_to_move
                && last_move_info[pile_index].moved_to
                && last_move_info[pile_index].depth > 0)) {
          continue;
        }
        if (len_tgt_pile) {
          tgt_card = piles_[tgt_index].exposed[len_tgt_pile-1];
        }
        else if (tgt_index + 1 > n_stock_left_) {
          // the point here is that empty piles that can no longer be
          // covered by a deal are all the same, so we only use the
          // first such pile and set a flag once such a pile is found
          found_empty_target = true;
        }

        // First handle the cases where the target pile is empty. The
        // first part of this long conditional is to make sure we are
        // only moving the whole pile if the source or target location
        // can be covered by a future deal.
        if (!len_tgt_pile
          && (n_to_move != len_curr_pile
            || !piles_[pile_index].hidden.empty()
            || (n_to_move == len_curr_pile
              && (tgt_index < n_stock_left_
                || pile_index < n_stock_left_)))
            &&
            (((move_to_empty_pile == EmptyRule::Any1
               || (move_to_empty_pile == EmptyRule::High1
                 && src_card.rank() == 12)) && n_to_move == 1)
               || ((move_to_empty_pile == EmptyRule::AnyRun
                || (move_to_empty_pile == EmptyRule::HighRun
                  && src_card.rank() == 12))))) {

          if (n_stock_left_
              || tabltype != TablType::Split || split_empty_stock
              || !(InFoundation(last_card.rank() - 1,
                               last_card.same_color_suit())
                    ||
                  (InFoundation(last_card.rank() - 2,
                               last_card.same_color_suit())
                   && InFoundation(last_card.rank() - 3,
                               last_card.suit())))) {
            reg_move = Move(Moves::InTableau, pile_index, src_card.suit(),
              n_to_move, tgt_index, expose, tabltype);
            valid_moves_.push_back(reg_move);
          }
        }
        else if (len_tgt_pile
            && src_card.rank() + 1 == tgt_card.rank()
            && src_card.IsSameColor(tgt_card)) {
          if (src_card.IsSameSuit(tgt_card)) {
            tabltype = TablType::Join;
          }

          if (n_stock_left_
              || tabltype != TablType::Split || split_empty_stock
              || !(InFoundation(last_card.rank() - 1,
                               last_card.same_color_suit())
                    ||
                  (InFoundation(last_card.rank() - 2,
                               last_card.same_color_suit())
                   && InFoundation(last_card.rank() - 3,
                               last_card.suit())))) {
            reg_move = Move(Moves::InTableau, pile_index, src_card.suit(),
                n_to_move, tgt_index, expose, tabltype);
            valid_moves_.push_back(reg_move);
          }

          // TODO:
          // These are the conditions under which we can force a move in the
          // tableau between cards of the same suit. We could probably be a bit
          // more aggressive in the case of move_same_suit (ie,
          // move_same_suit && next-suit src card in suit seq &&
          // Card(src_card.rank - 1, same suit) in foundation at least, but
          // probably more aggressive still. May be done in future
          if (!(n_stock_left_)
            && !split_empty_stock && src_card.IsSameSuit(tgt_card)
            && ( InFoundation(src_card.rank(), src_card.same_color_suit())
              || (!move_same_suit
                 && ( in_suit_seq_[src_card.rank()][src_card.same_color_suit()]
                    || (src_card.rank()<12
                      && (last_in_pile_[src_card.rank() + 1][
                                        src_card.same_color_suit()]
                          || src_in_next_suit_seq)))))) {
            //cout << "src_in_next_suit_seq=" << src_in_next_suit_seq << ", last_in_pile(next+1)=" << last_in_pile_[src_card.rank()+1][src_card.same_color_suit()] << '\n';
            force_move = true;
            forced_move = Move(Moves::InTableau, pile_index,
                src_card.suit(), n_to_move, tgt_index, expose,
                tabltype);
          }
        } // else if (len_tgt_pile ...)
      } // for tgt_index
    } // loop over n_movable

    // Move Type 2 - Put card onto foundation
    expose = (len_curr_pile == 1
          && piles_[pile_index].hidden.size());
    int suit = last_card.suit();
    if ((last_card.rank()-1) == foundation_[suit]
      && (track_threshold <= n_stock_left_
        || !last_move_info[pile_index].depth
        || last_move_info[pile_index].can_move_to_found)) {
      if (in_suit_seq_[last_card.rank()][last_card.suit()]) {
        tabltype = TablType::Split;
      }
      else {
        tabltype = TablType::None;
      }
      reg_move = Move(Moves::ToFoundation, pile_index, suit,
                      0, 0, expose, tabltype);
      valid_moves_.push_back(reg_move);
      int same_color_suit = ((suit+2)%4);
      if (InFoundation(last_card.rank() - 2, same_color_suit)
        || (!(n_stock_left_) && !split_empty_stock
          && in_suit_seq_[last_card.rank() - 1][
                     same_color_suit])) {
        force_move = true;
        forced_move = Move(Moves::ToFoundation, pile_index, suit,
                           0, 0, expose, tabltype);
      }
    }
  } // for pile_index

  if (force_move) {
    valid_moves_.clear();
    valid_moves_.push_back(forced_move);
  }
}

void AgnesState::UndoDealForPile(int pile_index) {
  AgnesPile& pile = piles_[pile_index];
  Card& last_card = pile.exposed.back();

  assert(!pile.exposed.empty());

  in_suit_seq_[last_card.rank()][last_card.suit()] = false;
  last_in_pile_[last_card.rank()][last_card.suit()] = false;
  pile.exposed.pop_back();
  if (pile.exposed.size()) {
    Card& new_last_card = pile.exposed.back();
    last_in_pile_[new_last_card.rank()][new_last_card.suit()] = true;
  }
  ++n_stock_left_;
}

void AgnesState::DealOntoPile(int pile_index, const Deck &deck,
                              const bool face_up) {
  AgnesPile& pile = piles_[pile_index];
  const Card& card = deck[kNCard - n_stock_left_];

  if (n_stock_left_ <= 0) throw;
  if (face_up) {
    if (pile.exposed.size()) {
      Card& last_card = pile.exposed.back();
      if (card.value() + 1 == last_card.value()) {
        in_suit_seq_[card.rank()][card.suit()] = true;
      }
      last_in_pile_[last_card.rank()][last_card.suit()] = false;
    }
    pile.exposed.push_back(card);
    last_in_pile_[card.rank()][card.suit()] = true;
  }
  else {
    pile.hidden.push_back(card);
  }
  --n_stock_left_;
}

void AgnesState::PlayBaseCard(const Card &card) {
  foundation_[card.suit()]=0;
  last_in_pile_[0][card.suit()]=true;
  in_suit_seq_[0][card.suit()]=true;
  --n_stock_left_;
}

void AgnesState::MoveToFoundation(const Move & curr_move,
                             std::array<LastMoveInfo, kNPile>& last_move_info,
                             const SetNMovableOpts & snm_opts,
                             const EmptyRule & enum_to_empty_pile) {
  assert(!piles_[curr_move.from].exposed.empty());
  assert(curr_move.expose ==
           (piles_[curr_move.from].exposed.size() == 1
            && !piles_[curr_move.from].hidden.empty()));

  ++depth_;
  Card last_card;

  // retrieve card and then destroy it
  last_card = piles_[curr_move.from].exposed.back();
  piles_[curr_move.from].exposed.pop_back();
  ++foundation_[last_card.suit()];
  if (curr_move.expose) {
    piles_[curr_move.from].exposed.push_back(
        piles_[curr_move.from].hidden.back());
    piles_[curr_move.from].hidden.pop_back();
  }

  if (piles_[curr_move.from].exposed.size()) {
    Card & new_last_card = piles_[curr_move.from].exposed.back();
    last_in_pile_[new_last_card.rank()][new_last_card.suit()] = true;
  }

  set_n_movable(curr_move.from, snm_opts);
  last_move_info[curr_move.from] = LastMoveInfo();

  if (curr_move.tabltype == TablType::None) {
    in_suit_seq_[last_card.rank()][last_card.suit()] = true;
  }
  // update sort order only if we emptied first pile
  if (!piles_[curr_move.from].exposed.size()) {
    set_sort_order(enum_to_empty_pile);
  }
}

void AgnesState::UndoMoveToFoundation(const Move & curr_move,
                             const SetNMovableOpts & snm_opts,
                             const EmptyRule & enum_to_empty_pile) {
  assert(depth_ > 0 && foundation_[curr_move.suit] >= 0);
  assert(!curr_move.expose || piles_[curr_move.from].exposed.size() == 1);

  --depth_;
  Card last_card (foundation_[curr_move.suit], curr_move.suit);
  if (curr_move.tabltype == TablType::None) {
    in_suit_seq_[last_card.rank()][last_card.suit()] = false;
  }
  if (!piles_[curr_move.from].exposed.empty()) {
    Card& card_to_hide = piles_[curr_move.from].exposed.back();
    last_in_pile_[card_to_hide.rank()][card_to_hide.suit()] = false;
  }
  if (curr_move.expose) {
    piles_[curr_move.from].hidden.push_back(
        piles_[curr_move.from].exposed.back());
    piles_[curr_move.from].exposed.pop_back();
  }
  piles_[curr_move.from].exposed.push_back(last_card);
  --foundation_[curr_move.suit];
  set_n_movable(curr_move.from, snm_opts);

  if (piles_[curr_move.from].exposed.size() == 1) {
    set_sort_order(enum_to_empty_pile);
  }
}

void AgnesState::DealMove(const Deck &deck,
                          std::array<LastMoveInfo, kNPile>& last_move_info,
                          const SetNMovableOpts & snm_opts,
                          const EmptyRule & enum_to_empty_pile) {
  assert(n_stock_left_ > 0);

  ++depth_;
  if (n_stock_left_ == 2) {
    DealOntoPile(0, deck, true);
    DealOntoPile(1, deck, true);

    // even though we only dealt to two piles, set_n_movable uses
    // n_stock_left variable, so we need to reset
    // n_movable for all piles, not just the first two.
    for (PileSizeType pile_index=0; pile_index<piles_.size(); ++pile_index) {
      set_n_movable(pile_index, snm_opts);
    }
    last_move_info[0] = LastMoveInfo();
    last_move_info[1] = LastMoveInfo();
  } else {
    for (PileSizeType pile_index=0; pile_index<piles_.size(); ++pile_index) {
      DealOntoPile(pile_index, deck, true);
    }
    // need two loops, as in_suit_seq needs to be set for all
    // cards before calling set_n_movable
    for (PileSizeType pile_index=0; pile_index< piles_.size(); ++pile_index) {
      set_n_movable(pile_index, snm_opts);
      last_move_info[pile_index] = LastMoveInfo();
    }
  }
  set_sort_order(enum_to_empty_pile);
}

void AgnesState::UndoDeal(const SetNMovableOpts & snm_opts,
                          const EmptyRule & enum_to_empty_pile) {
  assert(depth_ > 0 && n_stock_left_ <= 16);

  --depth_;
  if (n_stock_left_ == 0) {
    UndoDealForPile(0);
    UndoDealForPile(1);

    // even though we only undealt to two piles, the current
    // implementation of set_n_movable uses n_stock_left
    // variable, so we need to reset n_movable for all
    // piles, not just the first two.
    for (PileSizeType pile_index=0; pile_index<piles_.size(); ++pile_index) {
      set_n_movable(pile_index, snm_opts);
    }
  } else {
    for (PileSizeType pile_index=0; pile_index<piles_.size(); ++pile_index) {
      UndoDealForPile(pile_index);
    }
    // need two loops because need in_suit_seq assigned for
    // all cards before calling set_n_movable
    for (PileSizeType pile_index=0; pile_index<piles_.size(); ++pile_index) {
      set_n_movable(pile_index, snm_opts);
    }
  }
  // when undoing move, always update sort order, because it doesn't seem
  // worth it to check if any pile is being emptied.
  set_sort_order(enum_to_empty_pile);
}

void AgnesState::UndoTableauMove(const Move & curr_move,
                                 const SetNMovableOpts & snm_opts,
                                 const EmptyRule & enum_to_empty_pile) {
  assert(depth_ > 0
         && piles_[curr_move.to].exposed.size() >= curr_move.n_cards);
  assert(!curr_move.expose || piles_[curr_move.from].exposed.size() == 1);

  --depth_;
  std::vector<Card>& from_pile = piles_[curr_move.to].exposed;
  std::vector<Card>& to_pile = piles_[curr_move.from].exposed;
  int len_from_pile = from_pile.size();
  int len_to_pile = to_pile.size();

  Card & top_card = from_pile[len_from_pile - curr_move.n_cards];
  if (curr_move.tabltype == TablType::Join) {
    in_suit_seq_[top_card.rank()][top_card.suit()] = false;
  }
  else if (curr_move.tabltype == TablType::Split) {
    in_suit_seq_[top_card.rank()][top_card.suit()] = true;
  }

  if (len_from_pile != curr_move.n_cards) {
    Card& prev_card = from_pile[len_from_pile - curr_move.n_cards - 1];
    last_in_pile_[prev_card.rank()][prev_card.suit()] = true;
  }

  if (len_to_pile) {
    last_in_pile_[to_pile[len_to_pile-1].rank()][
                  to_pile[len_to_pile-1].suit()] = false;
  }

  // if putting card back on empty pile, need to update the sort order
  int pre_from_size = piles_[curr_move.from].exposed.size();
  if (curr_move.expose) {
    piles_[curr_move.from].hidden.push_back(
        piles_[curr_move.from].exposed.back());
    piles_[curr_move.from].exposed.pop_back();
  }

  int size_src_pile = piles_[curr_move.to].exposed.size();
  for (int n_to_pop=-1*curr_move.n_cards; n_to_pop < 0; ++n_to_pop) {
    piles_[curr_move.from].exposed.push_back(
        piles_[curr_move.to].exposed[size_src_pile+n_to_pop]);
  }

  // pop off the ones we just moved
  for (int n_to_pop=-1*curr_move.n_cards; n_to_pop < 0; ++n_to_pop) {
    piles_[curr_move.to].exposed.pop_back();
  }
  set_n_movable(curr_move.from, snm_opts);
  set_n_movable(curr_move.to, snm_opts);
  if (!pre_from_size || !piles_[curr_move.to].exposed.size()) {
    set_sort_order(enum_to_empty_pile);
  }
}

void AgnesState::TableauMove(const Move & curr_move,
                         std::array<LastMoveInfo, kNPile>& last_move_info,
                         const SetNMovableOpts & snm_opts,
                         const EmptyRule & enum_to_empty_pile) {
  assert(piles_[curr_move.from].exposed.size() >= curr_move.n_cards);
  assert(curr_move.expose ==
           (piles_[curr_move.from].exposed.size() == curr_move.n_cards
            && !piles_[curr_move.from].hidden.empty()));

  ++depth_;
  std::vector<Card>& from_pile = piles_[curr_move.from].exposed;
  std::vector<Card>& to_pile = piles_[curr_move.to].exposed;

  // update last_in_pile before we move.
  if (from_pile.size() != curr_move.n_cards) {
    Card& prev_card = from_pile[from_pile.size() - curr_move.n_cards - 1];
    last_in_pile_[prev_card.rank()][prev_card.suit()] = true;
  }
  else if (curr_move.expose) {
    Card& prev_card = piles_[curr_move.from].hidden.back();
    last_in_pile_[prev_card.rank()][prev_card.suit()] = true;
  }

  if (to_pile.size()) {
    last_in_pile_[to_pile[to_pile.size() - 1].rank()][
                  to_pile[to_pile.size() - 1].suit()] = false;
  }

  Card & top_card = from_pile[from_pile.size() - curr_move.n_cards];
  if (curr_move.tabltype == TablType::Join) {
    in_suit_seq_[top_card.rank()][top_card.suit()] = true;
  }
  else if (curr_move.tabltype == TablType::Split) {
    in_suit_seq_[top_card.rank()][top_card.suit()] = false;
  }

  Card & last_card = from_pile.back();

  last_move_info[curr_move.from].depth = depth_;
  last_move_info[curr_move.from].n_moved = curr_move.n_cards;
  last_move_info[curr_move.from].moved_to = false;
  last_move_info[curr_move.from].can_move_to_found = true;

  last_move_info[curr_move.to].depth = depth_;
  last_move_info[curr_move.to].n_moved = curr_move.n_cards;
  last_move_info[curr_move.to].moved_to = true;
  last_move_info[curr_move.to].can_move_to_found = (
    !(foundation_[last_card.suit()] == last_card.rank() - 1));

  int size_tgt_pre = to_pile.size();
  for (int n_to_pop=-1*curr_move.n_cards; n_to_pop < 0; ++n_to_pop) {
    int size_src_pile = from_pile.size();
    to_pile.push_back(from_pile[size_src_pile + n_to_pop]);
  }

  // pop off the ones we just moved
  for (int n_to_pop=-1*curr_move.n_cards; n_to_pop < 0; ++n_to_pop) {
    from_pile.pop_back();
  }
  if (curr_move.expose) {
    from_pile.push_back(piles_[curr_move.from].hidden.back());
    piles_[curr_move.from].hidden.pop_back();
  }

  set_n_movable(curr_move.from, snm_opts);
  set_n_movable(curr_move.to, snm_opts);

  if (!piles_[curr_move.from].exposed.size() || !size_tgt_pre) {
    set_sort_order(enum_to_empty_pile);
  }
}

bool pile_less(int n_stock_left, int int_left, const AgnesPile &left,
               int int_right, const AgnesPile &right) {
  if (n_stock_left == 2 && ((int_left <= 1) || (int_right <= 1))) {
    return (int_left < int_right);
  }
  else {
    int val_left;
    int val_right;
    if (left.hidden.size()) {
      val_left = left.hidden[0].value();
    }
    else if (left.exposed.size()) {
      val_left = left.exposed[0].value();
    }
    else {
      val_left = 90;
    }
    if (right.hidden.size()) {
      val_right = right.hidden[0].value();
    }
    else if (right.exposed.size()) {
      val_right = right.exposed[0].value();
    }
    else {
      val_right = 90;
    }
    if (val_left == 90 && val_right == 90) {
      return (int_left < int_right);
    }
    else {
      return (val_left < val_right);
    }
  }
}

void AgnesState::set_sort_order(EmptyRule move_to_empty_pile) {
  array<AgnesPile, kNPile> &piles = piles_;
  for (std::size_t i=0; i<sort_order_.size(); ++i) {
    sort_order_[i]=i;
  }
  const int n_stock_left = n_stock_left_;
  if (n_stock_left > 2) return;
  else if (move_to_empty_pile != EmptyRule::None) {
    std::sort(sort_order_.begin(), sort_order_.end(),
      [piles, n_stock_left](int left, int right) {
             return pile_less(n_stock_left, left, piles[left],
                    right, piles[right]); } );
  }
  //cout << "after sort:";
  //for (const int &val : sort_order_) { cout << val << ","; }
  //cout << "\n";
}


//------------------------------------------------------------------------------
// PRIVATE CLASS FUNCTIONS
//------------------------------------------------------------------------------
void AgnesState::PrintFoundation() const {
  cout << "Foundations:[";
  for (std::size_t i=0; i<foundation_.size()-1; ++i) {
    cout << foundation_[i] << ", ";
  }
  cout << foundation_[foundation_.size()-1] << "]\n";
}

void AgnesState::PrintValidMoves(const std::vector<Move> &valid_moves) const {
  cout << "[";
  for (auto it = valid_moves.begin(); it != valid_moves.end(); ++it) {
    if (it != valid_moves.begin()) {
      cout << ", ";
    }
    if (it->movetype == Moves::InTableau) {
      cout << "TablMove(from_=" << static_cast<int>(it->from);
      cout << ", n_cards=" << static_cast<int>(it->n_cards);
      cout << ", to_=" << static_cast<int>(it->to);
      if (it->expose) {
        cout << ", expose=True";
      }
      else {
        cout << ", expose=False";
      }
      cout << ", tabltype=";
      switch (it->tabltype)
      {
        case (TablType::None): cout << "0" ; break;
        case (TablType::Split): cout << "1" ; break;
        case (TablType::Join): cout << "2" ; break;
        default: cout << "Unexpected value"; break;
      }
      cout << ")";
    }
    else if (it->movetype == Moves::ToFoundation) {
      cout << "MoveToFound(from_=" << static_cast<int>(it->from);
      cout << ", suit=" << static_cast<int>(it->suit);
      if (it->expose) {
        cout << ", expose=True";
      }
      else {
        cout << ", expose=False";
      }
      cout << ", tabltype=";
      switch (it->tabltype)
      {
        case (TablType::None): cout << "0" ; break;
        case (TablType::Split): cout << "1" ; break;
        case (TablType::Join): cout << "2" ; break;
        default: cout << "Unexpected value"; break;
      }
      cout << ")";
    }
    else if (it->movetype == Moves::Deal) {
      cout << "DealMove()";
    }
  }
  cout << "]";
}

void AgnesState::PrintInSuitSeq() const {
  cout << "in_suit_seq:";
  for (int i=0; i<kNRank; ++i) {
    for (int j=0; j<kNSuit; ++j) {
      cout << in_suit_seq_[i][j];
    }
    if (i<kNRank-1) cout << " ";
  }
  cout << "\n";
  cout << "last_in_pile:";
  for (int i=0; i<kNRank; ++i) {
    for (int j=0; j<kNSuit; ++j) {
      cout << last_in_pile_[i][j];
    }
    if (i<kNRank-1) cout << " ";
  }
  cout << "\n";
}

void AgnesState::PrintTableau() const {
  for (PileSizeType i=0; i<piles_.size(); ++i) {
    cout << "T" << i << ":[";
    for (auto it = piles_[i].hidden.begin();
          it != piles_[i].hidden.end(); ++it) {
      if (it != piles_[i].hidden.begin()) { cout << ", " ; }
      cout << "(" << it->rank() << ", " << it->suit() << ")";
    }
    cout << "] | [";
    for (auto it = piles_[i].exposed.begin();
          it != piles_[i].exposed.end(); ++it) {
      if (it != piles_[i].exposed.begin()) { cout << ", " ; }
      cout << "(" << it->rank() << ", " << it->suit() << ")";
    }
    cout << "]" << "\n";
  }
}

void Card::set_value(int rank, int suit) {
  value_ = suit*16 + rank;
}

bool Card::IsSameSuit(Card& other) const {
  return (other.value_ >> 4) == (value_ >> 4);
}

bool Card::IsSameColor(Card& other) const {
  return (other.value_ & 0x10) == (value_ & 0x10);
}

bool Card::IsSameColor(const Card& other) const {
  return (other.value_ & 0x10) == (value_ & 0x10);
}

int Card::rank() const {
  return value_ & 0x0F;
}

uint8_t Card::value() const {
  return value_;
}

int Card::suit() const {
  return value_ >> 4;
}

// return (((value_ >> 4) + 2) % 4) is faster??
int Card::same_color_suit() const {
  return (value_ ^ 0x20) >> 4;
}

} // namespace quagnes
