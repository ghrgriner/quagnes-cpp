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
//------------------------------------------------------------------------------

#include <iostream>
#include <sstream>
#include <fstream>
#include <algorithm>

#include "AgnesState.h"

using std::array;
using std::cout;
using std::string;

namespace quagnes {

//------------------------------------------------------------------------------
// STATIC FUNCTION PROTOTYPES
//------------------------------------------------------------------------------
static bool visit(const int vertex,
       const std::array<std::array<bool, N_SUIT>, N_SUIT>& g,
       std::array<bool, N_SUIT>& current_path,
       std::array<bool, N_SUIT>& visited);
static bool cyclic(const std::array<std::array<bool, N_SUIT>, N_SUIT>& g);
static bool IsLastDealBlocked(const array<LastMoveInfo,
                              N_PILE>& last_move_info);

static string DescribeMove(const Move &move);
//bool pile_less(int n_stock_left, int int_left, const AgnesPile &left,
//         int int_right, const AgnesPile &right);

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
       const std::array<std::array<bool, N_SUIT>, N_SUIT>& g,
       std::array<bool, N_SUIT>& current_path,
       std::array<bool, N_SUIT>& visited) {
  if (visited[vertex]) return false;

  visited[vertex]=true;
  current_path[vertex]=true;

  for (int neighbor=0; neighbor<N_SUIT; ++neighbor) {
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
static bool cyclic(const std::array<std::array<bool, N_SUIT>, N_SUIT>& g) {
  std::array<bool, N_SUIT> current_path = {false, false, false, false};
  std::array<bool, N_SUIT> visited = {false, false, false, false};
  bool cycle = false;

  for (int v=0; v<N_SUIT; ++v) {
    cycle=visit(v, g, current_path, visited);
    if (cycle) return true;
  }
  return cycle;
}

//------------------------------------------------------------------------------
// PUBLIC CLASS FUNCTIONS
//------------------------------------------------------------------------------
AgnesState::AgnesState()
    : depth_(0),
      n_stock_left_(N_CARD),
      piles_(),
      curr_move_(),
      foundation_(),
      valid_moves_(),
      //sort_order_(),
      is_loop_(false),
      is_loser_(false),
      hash_()
{
  for (std::size_t i=0; i<foundation_.size(); ++i) {
    foundation_[i]=-1;
  }
  //for (std::size_t i=0; i<sort_order_.size(); ++i) {
  //  sort_order_[i] = i;
  //}
  for (int i=0; i<N_RANK; ++i) {
    for (int j=0; j<N_SUIT; ++j) {
      in_suit_seq_[i][j] = false;
      last_in_pile_[i][j] = false;
    }
  }
}

// Getters and simple setters
int AgnesState::depth() {
  return depth_;
}

uint8_t AgnesState::n_stock_left() {
  return n_stock_left_;
}

std::vector<Move> AgnesState::valid_moves() {
  return valid_moves_;
}

bool AgnesState::is_loop() {
  return is_loop_;
}

bool AgnesState::is_loser() {
  return is_loser_;
}

string AgnesState::hash() {
  return hash_;
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

void AgnesState::Print() {
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

string AgnesState::ToUncompStr() {
  std::ostringstream retoss;

  retoss << std::to_string(n_stock_left_);
  for (const AgnesPile& pile : piles_) {
    retoss << "#";
    for (auto it = pile.exposed.begin(); it != pile.exposed.end(); ++it) {
      if (it != pile.exposed.begin()) { retoss << "-"; }
      retoss << std::to_string((it->suit)*N_RANK+(it->rank));
    }
    retoss << "#";
    for (auto it = pile.hidden.begin(); it != pile.hidden.end(); ++it) {
      if (it != pile.hidden.begin()) { retoss << "-"; }
      retoss << std::to_string((it->suit)*N_RANK+(it->rank));
    }
  }
  return retoss.str();
}

void AgnesState::UpdateHash() {
  std::ostringstream retoss;

  retoss << (n_stock_left_);
  for (const AgnesPile& pile : piles_) {
    retoss << "#";
    for (auto it = pile.exposed.begin(); it != pile.exposed.end(); ++it) {
      retoss << std::to_string(48+(it->suit)*N_RANK+(it->rank));
    }
    retoss << "#";
    for (auto it = pile.hidden.begin(); it != pile.hidden.end(); ++it) {
      retoss << std::to_string(48+(it->suit)*N_RANK+(it->rank));
    }
  }
  hash_ = retoss.str();
}

bool AgnesState::IsAnyPileBlocked() {
  std::array<std::array<bool, N_SUIT>, N_SUIT> graph = {
      false, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false};

  for (const AgnesPile& pile: piles_) {
    PileSizeType hSize=pile.hidden.size();
    PileSizeType pSize=pile.exposed.size();
    if (hSize+pSize) {
      std::array<bool, N_SUIT> king_found = {false, false, false, false};
      // work backwards through the exposed file and then backwards
      // through the hidden pile
      for (PileSizeType j=0; j<pSize; ++j) {
        const Card &card = pile.exposed[pSize-1-j];
        if (card.rank == 12) {
          king_found[card.suit] = true;
        }
        for (int k=0; k<N_SUIT; ++k) {
          if (king_found[k] && ((card.rank<12) || (card.suit !=k))) {
            graph[k][card.suit] = true;
          }
        }
      } // end loop over exposed
      for (PileSizeType j=0; j<hSize; ++j) {
        const Card &card = pile.hidden[hSize-1-j];
        if (card.rank == 12) {
          king_found[card.suit] = true;
        }
        for (int k=0; k<N_SUIT; ++k) {
          if (king_found[k] && ((card.rank<12) || (card.suit !=k))) {
            graph[k][card.suit] = true;
          }
        }
      } // end loop over piles_.hidden
    }
  }
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
        above_rank = pile[card_index-1].rank;
        above_suit = pile[card_index-1].suit;
      }
      // the next two if statements verify that the card starts a
      // sequence of the same color
      if (!(card.rank - (max_index - card_index)
          == pile[pile_size-1].rank)) {
        break;
      }

      if (!(card.suit == pile[pile_size-1].suit)) {
        if (snm_opts.move_same_suit
            || !(card.suit % 2 == pile[pile_size-1].suit % 2))  {
          break;
        }
      }

      bool is_split_same_suit=(above_rank == card.rank + 1
                   && above_suit == card.suit);
      bool is_split_same_color=(above_rank == card.rank + 1
                    && above_suit % 2 == card.suit % 2);

      // Lastly, only add to n_movable if we are allowed to split runs
      // (conditional line 1) or the move is not splitting a run
      // (remaining lines of conditional).
      if (snm_opts.split_runs
        || (snm_opts.move_same_suit && !is_split_same_suit)
        || (!snm_opts.move_same_suit && !is_split_same_color)) {

        // But... we should also never split between two suits in
        // sequence when the stock is 0 and split_empty_stock=false.
        // run (regardless of whether we are moving by suit or color)
        if (!(n_stock_left_) && !snm_opts.split_empty_stock
          && is_split_same_suit) continue;

        n_movable.push_back( (max_index - card_index) + 1);
      }
    }
  }
}

// Return true if piles 2-6 (using zero-indexing) had a move
static bool IsLastDealBlocked(const array<LastMoveInfo,
                              N_PILE>& last_move_info) {
  //return false;
  std::set<int> anydup;
  for (std::size_t i=2; i<N_PILE; ++i) {
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

void AgnesState::set_valid_moves(EmptyRule move_to_empty_pile,
      const bool move_same_suit,
      const bool split_empty_stock,
      const int track_threshold,
      const array<LastMoveInfo, N_PILE>& last_move_info) {
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
  }

  for (PileSizeType pile_index=0; pile_index<piles_.size(); ++pile_index) {
    bool expose = false;
    std::vector<int>& n_movable = piles_[pile_index].n_movable;
    int len_curr_pile = piles_[pile_index].exposed.size();
    TablType tabltype = TablType::None;
    for (const int& n_to_move : n_movable) {
      tabltype = TablType::None;
      const Card &src_card = piles_[pile_index].exposed[
          len_curr_pile - n_to_move];
      if (n_to_move == len_curr_pile) {
        expose = piles_[pile_index].hidden.size();
      }
      else {
        expose = false;
        const Card &card_above = piles_[pile_index].exposed[
          len_curr_pile - n_to_move - 1];
        if (src_card.rank + 1 == card_above.rank
            && src_card.suit == card_above.suit) {
          tabltype = TablType::Split;
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
            || (n_to_move == len_curr_pile
              && (tgt_index < n_stock_left_
                || pile_index < n_stock_left_)))
            &&
            (((move_to_empty_pile == EmptyRule::Any1
               || (move_to_empty_pile == EmptyRule::High1
                 && src_card.rank == 12)) && n_to_move == 1)
               || ((move_to_empty_pile == EmptyRule::AnyRun
                || (move_to_empty_pile == EmptyRule::HighRun
                  && src_card.rank == 12))))) {
          if (pile_index < n_stock_left_ || tgt_index < n_stock_left_
              || tabltype != TablType::Split || split_empty_stock
              || foundation_[((src_card.suit + 2) % 4)] < src_card.rank) {
            reg_move = Move(Moves::InTableau, pile_index, src_card.suit,
              n_to_move, tgt_index, expose, tabltype);
            valid_moves_.push_back(reg_move);
          }
        }
        else if (len_tgt_pile
            && src_card.rank == (tgt_card.rank-1)
            && ((src_card.suit % 2) == (tgt_card.suit % 2))) {
          if (src_card.suit == tgt_card.suit) {
            tabltype = TablType::Join;
          }
          if (pile_index < n_stock_left_ || tgt_index < n_stock_left_
              || tabltype != TablType::Split || split_empty_stock
              || foundation_[((src_card.suit + 2) % 4)] < src_card.rank) {
            reg_move = Move(Moves::InTableau, pile_index, src_card.suit,
                n_to_move, tgt_index, expose, tabltype);
            valid_moves_.push_back(reg_move);
          }
          // if there's no stock left, force move under the same suit,
          // as long as !split_empty_stock and same-color suit
          // equal-ranked card is already in sequence or in the
          // foundation
          if (!(n_stock_left_)
            && !split_empty_stock && tgt_card.suit == src_card.suit
            && ( foundation_[(src_card.suit + 2) % 4] + 1
               >= src_card.rank
              || (!move_same_suit
                 && ( in_suit_seq_[src_card.rank][
                     ((src_card.suit + 2) % 4)]
                    || (src_card.rank<12
                      && last_in_pile_[
                        src_card.rank + 1][
                        ((src_card.suit + 2) % 4)] ))))) {
            force_move=true;
            forced_move = Move(Moves::InTableau, pile_index,
                src_card.suit, n_to_move, tgt_index, expose,
                tabltype);
          }
        } // else if (len_tgt_pile ...)
      } // for tgt_index
    } // loop over n_movable

    if (len_curr_pile) {
      // Move Type 2 - Put card onto foundation
      //p_last_card = &(piles_[pile_index].exposed[len_curr_pile-1]);
      expose = (len_curr_pile == 1
            && piles_[pile_index].hidden.size());
      const Card &last_card =
          (piles_[pile_index].exposed[len_curr_pile-1]);
      int suit = last_card.suit;
      if ((last_card.rank-1) == foundation_[suit]
        && (track_threshold <= n_stock_left_
          || !last_move_info[pile_index].depth
          || last_move_info[pile_index].can_move_to_found)) {
        if (in_suit_seq_[last_card.rank][last_card.suit]) {
          tabltype = TablType::Split;
        }
        else {
          tabltype = TablType::None;
        }
        reg_move = Move(Moves::ToFoundation, pile_index, suit,
                        0, 0, expose, tabltype);
        valid_moves_.push_back(reg_move);
        int same_color_suit = ((suit+2)%4);
        if (last_card.rank <= foundation_[same_color_suit]+2
          || (!(n_stock_left_) && !split_empty_stock
            && in_suit_seq_[last_card.rank-1][
                       same_color_suit])) {
          force_move=true;
          forced_move = Move(Moves::ToFoundation, pile_index, suit,
                             0, 0, expose, tabltype);
        }
      }
    } // if pile not empty
  } // for pile_index

  if (force_move) {
    valid_moves_.clear();
    valid_moves_.push_back(forced_move);
  }
}

void AgnesState::UndoDealForPile(int pile_index) {
  AgnesPile& pile = piles_[pile_index];
  Card& last_card = pile.exposed.back();
  in_suit_seq_[last_card.rank][last_card.suit] = false;
  last_in_pile_[last_card.rank][last_card.suit] = false;
  pile.exposed.pop_back();
  if (pile.exposed.size()) {
    Card& new_last_card = pile.exposed.back();
    last_in_pile_[new_last_card.rank][new_last_card.suit] = true;
  }
  ++n_stock_left_;
}

void AgnesState::DealOntoPile(int pile_index, const Deck &deck,
                              const bool face_up) {
  AgnesPile& pile = piles_[pile_index];
  const Card& card = deck[N_CARD - n_stock_left_];

  if (n_stock_left_ <= 0) throw;
  if (face_up) {
    if (pile.exposed.size()) {
      Card& last_card = pile.exposed.back();
      if ((card.rank + 1 == last_card.rank)
          && (card.suit == last_card.suit)) {
        in_suit_seq_[card.rank][card.suit] = true;
      }
      last_in_pile_[last_card.rank][last_card.suit] = false;
    }
    pile.exposed.push_back(card);
    last_in_pile_[card.rank][card.suit] = true;
  }
  else {
    pile.hidden.push_back(card);
  }
  --n_stock_left_;
}

void AgnesState::PlayBaseCard(const Card &card) {
  foundation_[card.suit]=0;
  last_in_pile_[0][card.suit]=true;
  in_suit_seq_[0][card.suit]=true;
  --n_stock_left_;
}

void AgnesState::MoveToFoundation(const Move & curr_move,
                             std::array<LastMoveInfo, N_PILE>& last_move_info,
                             const SetNMovableOpts & snm_opts) {
  ++depth_;
  Card last_card;
  // retrieve card and then destroy it
  last_card = piles_[curr_move.from].exposed.back();
  piles_[curr_move.from].exposed.pop_back();
  ++foundation_[last_card.suit];
  if (curr_move.expose) {
    piles_[curr_move.from].exposed.push_back(
        piles_[curr_move.from].hidden.back());
    piles_[curr_move.from].hidden.pop_back();
  }

  if (piles_[curr_move.from].exposed.size()) {
    Card & new_last_card = piles_[curr_move.from].exposed.back();
    last_in_pile_[new_last_card.rank][new_last_card.suit] = true;
  }

  set_n_movable(curr_move.from, snm_opts);
  last_move_info[curr_move.from] = LastMoveInfo();

  if (curr_move.tabltype == TablType::None) {
    in_suit_seq_[last_card.rank][last_card.suit] = true;
  }
  // update sort order only if we emptied first pile
  //if (!curr_state_.piles_[curr_move.from].exposed.size()) {
    //curr_state_.set_sort_order(enum_to_empty_pile_);
  //}
}

void AgnesState::UndoMoveToFoundation(const Move & curr_move,
                                 const SetNMovableOpts & snm_opts) {
  --depth_;
  Card last_card (foundation_[curr_move.suit], curr_move.suit);
  if (curr_move.tabltype == TablType::None) {
    in_suit_seq_[last_card.rank][last_card.suit] = false;
  }
  if (!piles_[curr_move.from].exposed.empty()) {
    Card& card_to_hide = piles_[curr_move.from].exposed.back();
    last_in_pile_[card_to_hide.rank][card_to_hide.suit] = false;
  }
  if (curr_move.expose) {
    piles_[curr_move.from].hidden.push_back(
        piles_[curr_move.from].exposed.back());
    piles_[curr_move.from].exposed.pop_back();
  }
  piles_[curr_move.from].exposed.push_back(last_card);
  --foundation_[curr_move.suit];
  set_n_movable(curr_move.from, snm_opts);
  //if (!curr_state_.piles_[curr_move.from].exposed.size()) {
    //curr_state_.set_sort_order(enum_to_empty_pile_);
  //}
}

void AgnesState::DealMove(const Deck &deck,
                          std::array<LastMoveInfo, N_PILE>& last_move_info,
                          const SetNMovableOpts & snm_opts) {
  ++depth_;
  if (n_stock_left() == 2) {
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
}

void AgnesState::UndoDeal(const SetNMovableOpts & snm_opts) {
  --depth_;
  if (n_stock_left() == 0) {
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
  //set_sort_order(enum_to_empty_pile_);
}

void AgnesState::UndoTableauMove(const Move & curr_move,
                            const SetNMovableOpts & snm_opts) {
  --depth_;
  std::vector<Card>& from_pile = piles_[curr_move.to].exposed;
  std::vector<Card>& to_pile = piles_[curr_move.from].exposed;
  int len_from_pile = from_pile.size();
  int len_to_pile = to_pile.size();

  Card & top_card = from_pile[len_from_pile - curr_move.n_cards];
  if (curr_move.tabltype == TablType::Join) {
    in_suit_seq_[top_card.rank][top_card.suit] = false;
  }
  else if (curr_move.tabltype == TablType::Split) {
    in_suit_seq_[top_card.rank][top_card.suit] = true;
  }

  if (len_from_pile != curr_move.n_cards) {
    Card& prev_card = from_pile[len_from_pile - curr_move.n_cards - 1];
    last_in_pile_[prev_card.rank][prev_card.suit] = true;
  }

  if (len_to_pile) {
    last_in_pile_[to_pile[len_to_pile-1].rank][
                  to_pile[len_to_pile-1].suit] = false;
  }

  // if putting card back on empty pile, need to update the sort order
  //int pre_from_size = curr_state_.piles_[curr_move.from].exposed.size();
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
  //if (!pre_from_size
  //    || !piles_[curr_move.to].exposed.size()) {
  //  piles_.set_sort_order(enum_to_empty_pile_);
  //}
}

void AgnesState::TableauMove(const Move & curr_move,
                         std::array<LastMoveInfo, N_PILE>& last_move_info,
                         const SetNMovableOpts & snm_opts) {
  ++depth_;
  std::vector<Card>& from_pile = piles_[curr_move.from].exposed;
  std::vector<Card>& to_pile = piles_[curr_move.to].exposed;

  // update last_in_pile before we move.
  if (from_pile.size() != curr_move.n_cards) {
    Card& prev_card = from_pile[from_pile.size() - curr_move.n_cards - 1];
    last_in_pile_[prev_card.rank][prev_card.suit] = true;
  }
  if (to_pile.size()) {
    last_in_pile_[to_pile[to_pile.size() - 1].rank][
                  to_pile[to_pile.size() - 1].suit] = false;
  }

  Card & top_card = from_pile[from_pile.size() - curr_move.n_cards];
  if (curr_move.tabltype == TablType::Join) {
    in_suit_seq_[top_card.rank][top_card.suit] = true;
  }
  else if (curr_move.tabltype == TablType::Split) {
    in_suit_seq_[top_card.rank][top_card.suit] = false;
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
    !(foundation_[last_card.suit] == last_card.rank - 1));

  //int size_tgt_pre = to_pile.size();
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

  //if (!piles_[from_].exposed.size() || !size_tgt_pre) {
  //  set_sort_order(enum_to_empty_pile_);
  //}
}
/*
bool pile_less(int n_stock_left, int int_left, const AgnesPile &left,
         int int_right, const AgnesPile &right)
{
  if (n_stock_left == 2 && ((int_left <= 1) || (int_right <= 1))) {
    return (int_left < int_right);
  }
  else {
    int val_left;
    int val_right;
    if (left.hidden.size()) {
      val_left = left.hidden[0].rank*4+left.hidden[0].suit + 1;
    }
    else if (left.exposed.size()) {
      val_left = left.exposed[0].rank*4+left.exposed[0].suit + 1;
    }
    else {
      val_left = 0;
    }
    if (right.hidden.size()) {
      val_right = right.hidden[0].rank*4+right.hidden[0].suit + 1;
    }
    else if (right.exposed.size()) {
      val_right = right.exposed[0].rank*4+right.exposed[0].suit + 1;
    }
    else {
      val_right = 0;
    }
    if (!val_left && !val_right) {
      return (int_left < int_right);
    }
    else {
      return (val_left < val_right);
    }
  }
}

void AgnesState::set_sort_order(EmptyRule move_to_empty_pile)
{
  array<AgnesPile, N_PILE> &piles = piles;
  for (std::size_t i=0; i<sort_order.size(); ++i) {
    sort_order_[i]=i;
  }
  const int n_stock_left = n_stock_left_;
  if (n_stock_left > 2) return;
  else if (move_to_empty_pile != EmptyRule::None) {
    std::sort(sort_order_.begin(), sort_order_.end(),
      [piles, n_stock_left](int left, int right) {
             return pile_less(n_stock_left, left, piles_[left],
                    right, piles_[right]); } );
  }
  //cout << "after sort:";
  //for (const int &val : sort_order_) { cout << val << ","; }
  //cout << "\n";
}
*/

//------------------------------------------------------------------------------
// PRIVATE CLASS FUNCTIONS
//------------------------------------------------------------------------------
void AgnesState::PrintFoundation()
{
  cout << "Foundations:[";
  for (std::size_t i=0; i<foundation_.size()-1; ++i) {
    cout << foundation_[i] << ", ";
  }
  cout << foundation_[foundation_.size()-1] << "]\n";
}

void AgnesState::PrintValidMoves(const std::vector<Move> &valid_moves) {
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

void AgnesState::PrintInSuitSeq() {
  cout << "in_suit_seq:";
  for (int i=0; i<N_RANK; ++i) {
    for (int j=0; j<N_SUIT; ++j) {
      cout << in_suit_seq_[i][j];
    }
    if (i<N_RANK-1) cout << " ";
  }
  cout << "\n";
  cout << "last_in_pile:";
  for (int i=0; i<N_RANK; ++i) {
    for (int j=0; j<N_SUIT; ++j) {
      cout << last_in_pile_[i][j];
    }
    if (i<N_RANK-1) cout << " ";
  }
  cout << "\n";
}

void AgnesState::PrintTableau() {
  for (PileSizeType i=0; i<piles_.size(); ++i) {
    cout << "T" << i << ":[";
    for (auto it = piles_[i].hidden.begin();
          it != piles_[i].hidden.end(); ++it) {
      if (it != piles_[i].hidden.begin()) { cout << ", " ; }
      cout << "(" << it->rank << ", " << it->suit << ")";
    }
    cout << "] | [";
    for (auto it = piles_[i].exposed.begin();
          it != piles_[i].exposed.end(); ++it) {
      if (it != piles_[i].exposed.begin()) { cout << ", " ; }
      cout << "(" << it->rank << ", " << it->suit << ")";
    }
    cout << "]" << "\n";
  }
}

} // namespace quagnes
