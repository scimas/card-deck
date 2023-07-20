use std::{collections::HashSet, fmt::Display};

use crate::Deck;

/// A playing card.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Card {
    /// A card with a suit and a rank.
    Normal { suit: Suit, rank: Rank },
    /// A wildcard, sometimes also known as Joker or trump card.
    Wildcard,
}

impl Card {
    /// Creat a "normal" playing card with the given `suit` and `rank`.
    pub fn new_normal(suit: Suit, rank: Rank) -> Self {
        Card::Normal { suit, rank }
    }

    /// Create a wildcard playing ... card.
    ///
    /// Also known as a Joker.
    pub fn new_wildcard() -> Self {
        Card::Wildcard
    }

    /// Get the `Rank` of the card if it is a normal card, `None` otherwise.
    pub fn rank(&self) -> Option<&Rank> {
        match self {
            Card::Normal { rank, .. } => Some(rank),
            Card::Wildcard => None,
        }
    }

    /// Get the `Suit` of the card if it is a normal card, `None` otherwise.
    pub fn suit(&self) -> Option<&Suit> {
        match self {
            Card::Normal { suit, .. } => Some(suit),
            Card::Wildcard => None,
        }
    }
}

/// Suits of the "normal" playing cards.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Suit {
    Clubs,
    Diamonds,
    Hearts,
    Spades,
}

impl Suit {
    /// Get all suits as a `Vec`.
    pub fn all_suits() -> [Suit; 4] {
        [Suit::Clubs, Suit::Diamonds, Suit::Hearts, Suit::Spades]
    }

    pub fn name(&self) -> &str {
        match self {
            Suit::Clubs => "clubs",
            Suit::Diamonds => "diamonds",
            Suit::Hearts => "hearts",
            Suit::Spades => "spades",
        }
    }
}

/// Ranks of the "normal" playing cards.
///
/// Numeric are from 2 to 10 (inclusive) along with Jack, Queen, King and Ace.
/// Ordering is not imposed on the ranks, which rank is smaller or bigger should
/// be decided by the caller.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Rank {
    Numeric(u8),
    Jack,
    Queen,
    King,
    Ace,
}

impl Rank {
    /// Create a new `Rank` from a numeric value, where `2..=10` correspond to the
    /// respective ranks, 1, 11, 12, 13 correspond to the Ace, the Jack, the
    /// Queen and the King respectively.
    ///
    /// # Panics
    /// Panics if the provided value is not in the above specified range.
    ///
    /// # Example
    /// ```rust
    /// use card_deck::standard_deck::Rank;
    ///
    /// let rank = Rank::new(11);
    /// assert_eq!(rank, Rank::Jack);
    /// ```
    pub fn new(value: u8) -> Rank {
        match value {
            0 | 14.. => panic!("rank value not in range"),
            2..=10 => Rank::Numeric(value),
            1 => Rank::Ace,
            11 => Rank::Jack,
            12 => Rank::Queen,
            13 => Rank::King,
        }
    }

    /// Get all ranks.
    pub fn all_ranks() -> HashSet<Rank> {
        (1..=13).map(|value| Rank::new(value)).collect()
    }

    /// Get all numeric ranks.
    pub fn all_numeric() -> HashSet<Rank> {
        (2..=10).map(|value| Rank::new(value)).collect()
    }

    /// Get all face ranks, this includes [`Rank::Ace`].
    pub fn all_face() -> HashSet<Rank> {
        HashSet::from([Rank::Jack, Rank::Queen, Rank::King, Rank::Ace])
    }

    /// Get the "value" of a `Rank`.
    ///
    /// # Example
    /// ```rust
    /// use card_deck::standard_deck::Rank;
    ///
    /// let rank = Rank::new(11);
    /// assert_eq!(rank.value(), 11);
    /// ```
    pub fn value(&self) -> u8 {
        match self {
            Rank::Numeric(val) => *val,
            Rank::Jack => 11,
            Rank::Queen => 12,
            Rank::King => 13,
            Rank::Ace => 1,
        }
    }
}

/// A builder for creating decks of standard cards.
///
/// Which cards to be included or excluded can be specified. Only one of
/// `include` or `exclude` can be specified. Setting either one will nullify the
/// other.
///
/// The default deck has 13 cards in each of the 4 suits and no wildcards
/// for a total of 52 cards. The suits are Clubs, Diamonds, Hearts and
/// Spades. The thirteen ranks are 2 to 10 (inclusive), Jack, Queen, King
/// and Ace.
///
/// Number of repetitions of the basic deck can be specified using `subdecks`
/// (default 1). This does not include wildcards. Wildcards are added after the
/// rest of the deck has been built.
///
/// Number of wildcards can be specified using `wildcards` (default 0).
pub struct StandardDeckBuilder {
    // "Normal" cards to include
    include: Option<HashSet<Card>>,
    // "Normal" cards to exclude
    // Must specify only one of `include` and `exclude`
    exclude: Option<HashSet<Card>>,
    // Repetitions of the deck
    num_subdecks: Option<usize>,
    // Number of wildcards to include in the deck
    num_wildcards: Option<usize>,
}

impl StandardDeckBuilder {
    /// Create the default deck builder.
    pub fn new() -> StandardDeckBuilder {
        Self {
            include: None,
            exclude: None,
            num_subdecks: Some(1),
            num_wildcards: Some(0),
        }
    }

    /// Cards to include in the deck.
    ///
    /// Will set exclude to `None`.
    pub fn include_cards(mut self, cards: HashSet<Card>) -> StandardDeckBuilder {
        self.include = Some(cards);
        self.exclude = None;
        self
    }

    /// Cards to exclude from the deck.
    ///
    /// Will set include to `None`.
    pub fn exclude_cards(mut self, cards: HashSet<Card>) -> StandardDeckBuilder {
        self.exclude = Some(cards);
        self.include = None;
        self
    }

    /// Set repetitions of the basic deck.
    ///
    /// This does not include repetition of wildcards. Wildcards are added after
    /// the rest of the deck has been built.
    ///
    /// # Panics
    /// Panics if the repetitions are not a positive integer.
    pub fn subdecks(mut self, num: usize) -> StandardDeckBuilder {
        assert!(num >= 1, "num must be a positive integer");
        self.num_subdecks = Some(num);
        self
    }

    /// Set the number of wildcards to include in the deck.
    ///
    /// These are added after the rest of the deck has been built and as such
    /// are not duplicated with the rest of the deck even if subdecks > 1.
    pub fn wildcards(mut self, num: usize) -> StandardDeckBuilder {
        self.num_wildcards = Some(num);
        self
    }

    /// Build the deck with the selected options.
    pub fn build(self) -> Deck<Card> {
        let basic_deck = match (self.include, self.exclude) {
            (Some(cards), None) => cards,
            (None, Some(cards)) => full_deck().difference(&cards).cloned().collect(),
            (None, None) => full_deck(),
            (Some(_), Some(_)) => unreachable!("construction constraints do not allow this state"),
        };
        let mut deck: Vec<_> = if let Some(n) = self.num_subdecks {
            basic_deck
                .into_iter()
                .flat_map(|card| std::iter::repeat(card).take(n))
                .collect()
        } else {
            basic_deck.into_iter().collect()
        };
        if let Some(n) = self.num_wildcards {
            (0..n).for_each(|_| deck.push(Card::new_wildcard()));
        }
        Deck::new(deck)
    }
}

impl Default for StandardDeckBuilder {
    fn default() -> Self {
        Self::new()
    }
}

fn cards_for_suit(suit: &Suit) -> HashSet<Card> {
    (1..=13u8)
        .map(|rank| Card::new_normal(*suit, Rank::new(rank)))
        .collect()
}

fn full_deck() -> HashSet<Card> {
    [Suit::Clubs, Suit::Diamonds, Suit::Hearts, Suit::Spades]
        .into_iter()
        .flat_map(|suit| cards_for_suit(&suit))
        .collect()
}

impl Display for Suit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Suit::Clubs => write!(f, "\u{2663}"),
            Suit::Diamonds => write!(f, "\u{2666}"),
            Suit::Hearts => write!(f, "\u{2665}"),
            Suit::Spades => write!(f, "\u{2660}"),
        }
    }
}

impl Display for Rank {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Rank::Numeric(n @ 2..=10) => write!(f, "{n}"),
            Rank::Jack => write!(f, "J"),
            Rank::Queen => write!(f, "Q"),
            Rank::King => write!(f, "K"),
            Rank::Ace => write!(f, "A"),
            Rank::Numeric(_) => Err(std::fmt::Error),
        }
    }
}

impl Display for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Card::Wildcard => write!(f, "\u{1f0cf}"),
            Card::Normal { suit, rank } => {
                let start = match suit {
                    Suit::Clubs => 0x1f0d0u32,
                    Suit::Diamonds => 0x1f0c0,
                    Suit::Hearts => 0x1f0b0,
                    Suit::Spades => 0x1f0a0,
                };
                let char_value = start
                    + match rank {
                        Rank::Numeric(n @ 2..=10) => u32::from(*n),
                        Rank::Jack => 11,
                        Rank::Queen => 13,
                        Rank::King => 14,
                        Rank::Ace => 1,
                        Rank::Numeric(_) => return Err(std::fmt::Error),
                    };
                let s = char::from_u32(char_value).ok_or(std::fmt::Error)?;
                write!(f, "{s}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::iter;

    use super::{cards_for_suit, full_deck, Rank, Suit};

    #[test]
    fn test_rank_in_range_success() {
        for value in 1..=13u8 {
            let rank = Rank::new(value);
            assert_eq!(rank.value(), value)
        }
    }

    #[test]
    #[should_panic]
    fn test_rank_out_of_range_fail() {
        for value in iter::once(0).chain(14..=u8::MAX) {
            Rank::new(value);
        }
    }

    #[test]
    fn test_thirteen_cards_for_each_suit() {
        for suit in [Suit::Clubs, Suit::Diamonds, Suit::Hearts, Suit::Spades] {
            let cards = cards_for_suit(&suit);
            assert_eq!(cards.len(), 13);
        }
    }

    #[test]
    fn test_fifty_two_cards_in_deck() {
        let deck = full_deck();
        assert_eq!(deck.len(), 52usize);
    }
}
