use rand::{prelude::SliceRandom, Rng};

#[cfg(feature = "standard-deck")]
pub mod standard_deck;

/// A deck of cards.
#[derive(Debug)]
pub struct Deck<C> {
    cards: Vec<C>,
}

impl<C: PartialEq> PartialEq for Deck<C> {
    fn eq(&self, other: &Self) -> bool {
        self.cards == other.cards
    }
}

impl<C: PartialEq> Eq for Deck<C> {}

impl<C> From<Vec<C>> for Deck<C> {
    fn from(cards: Vec<C>) -> Self {
        Deck::new(cards)
    }
}

impl<C> Deck<C> {
    /// Create a new deck with the given `cards` and the random number generator
    /// `rng`.
    pub fn new(cards: Vec<C>) -> Self {
        Self { cards }
    }

    /// Create an empty deck with optionally provided `rng`.
    pub fn new_empty() -> Self {
        Self { cards: vec![] }
    }

    /// Add a `card` to the deck.
    pub fn add(&mut self, card: C) {
        self.cards.push(card);
    }

    /// Draw a card from the deck or `None` if the deck is empty.
    pub fn draw(&mut self) -> Option<C> {
        self.cards.pop()
    }

    pub fn draw_n(&mut self, n: usize) -> impl Iterator<Item = C> + '_ {
        self.cards.drain(0..n)
    }

    /// Shuffle the deck.
    pub fn shuffle<R: Rng>(&mut self, rng: &mut R) {
        self.cards.shuffle(rng);
    }

    /// Sort the deck with the given comparator.
    pub fn sort_by<F>(&mut self, compare: F)
    where
        F: FnMut(&C, &C) -> std::cmp::Ordering,
    {
        self.cards.sort_by(compare)
    }

    /// Create an `Iterator` over the cards of this deck.
    pub fn iter(&self) -> impl Iterator<Item = &C> {
        self.cards.iter()
    }
}

impl<C> IntoIterator for Deck<C> {
    type Item = C;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    /// Create an iterator over the `Card`s in the deck.
    fn into_iter(self) -> Self::IntoIter {
        self.cards.into_iter()
    }
}
