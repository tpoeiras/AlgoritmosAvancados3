use std::mem;
use std::time::Instant;

use rand::prelude::*;
use rand::rngs::StdRng;
use rand::seq::SliceRandom;

#[derive(Debug)]
struct HeapEntry<K, D> {
    key: K,
    data: D,
}

trait Heap<K: Ord, D> {
    type EntryRef;
    fn insert(&mut self, entry: HeapEntry<K, D>) -> Self::EntryRef;
    fn delete_min(&mut self) -> Option<HeapEntry<K, D>>;
}

struct BinaryHeap<K, D> {
    storage: Vec<HeapEntry<K, D>>,
}

impl<K, D> BinaryHeap<K, D> {
    fn new() -> BinaryHeap<K, D> {
        BinaryHeap {
            storage: Vec::new(),
        }
    }
}

trait DecreaseKeyHeap<K: Ord, D>: Heap<K, D> {
    fn decrease_key(&mut self, reference: Self::EntryRef, new_key: K);
}

impl<K: Ord, D> Heap<K, D> for BinaryHeap<K, D> {
    type EntryRef = usize;

    fn insert(&mut self, entry: HeapEntry<K, D>) -> usize {
        self.storage.push(entry);

        let mut current_index = self.storage.len() - 1;
        while current_index != 0 {
            let parent_index = (current_index - 1) / 2;

            if self.storage[current_index].key < self.storage[parent_index].key {
                self.storage.swap(current_index, parent_index);
                current_index = parent_index;
            } else {
                break;
            }
        }

        current_index
    }

    fn delete_min(&mut self) -> Option<HeapEntry<K, D>> {
        if self.storage.len() <= 1 {
            let val = self.storage.pop();
            return val;
        }

        let len = self.storage.len();
        self.storage.swap(0, len - 1);
        let root = self.storage.pop();

        let mut current_index = 0;
        loop {
            let child_index = 2 * current_index + 1;

            if child_index < self.storage.len() {
                let max_child_index = if child_index + 1 < self.storage.len() {
                    child_index + 1
                } else {
                    child_index
                };

                let childs = &self.storage[child_index..=max_child_index];
                let max_index = child_index
                    + childs
                        .iter()
                        .enumerate()
                        .min_by_key(|a| &a.1.key)
                        .unwrap()
                        .0;

                if self.storage[current_index].key > self.storage[max_index].key {
                    self.storage.swap(current_index, max_index);
                    current_index = max_index;
                    continue;
                }
            }

            break;
        }

        root
    }
}

impl<K: Ord, D> DecreaseKeyHeap<K, D> for BinaryHeap<K, D> {
    fn decrease_key(&mut self, reference: usize, new_key: K) {
        let mut current_index = reference;
        self.storage[current_index].key = new_key;

        while current_index != 0 {
            let parent_index = (current_index - 1) / 2;

            if self.storage[current_index].key < self.storage[parent_index].key {
                self.storage.swap(current_index, parent_index);
                current_index = parent_index;
            } else {
                break;
            }
        }
    }
}

#[derive(Debug)]
struct BinomialTree<K, D> {
    root: HeapEntry<K, D>,
    childs: Vec<BinomialTree<K, D>>,
}

impl<K: Ord, D> BinomialTree<K, D> {
    fn merge(&mut self, mut other: Self) {
        if self.root.key > other.root.key {
            mem::swap(self, &mut other);
        }

        self.childs.push(other);
    }
}

#[derive(Debug)]
struct BinomialHeap<K, D> {
    ranks: Vec<Option<BinomialTree<K, D>>>,
}

impl<K: Ord, D> BinomialHeap<K, D> {
    fn new() -> BinomialHeap<K, D> {
        BinomialHeap { ranks: Vec::new() }
    }

    fn merge(&mut self, mut other: Self) {
        if other.ranks.len() > self.ranks.len() {
            mem::swap(self, &mut other);
        }

        let mut carry_tree = None;
        let other_rank = other.ranks.len();
        for (rank, other_tree) in other.ranks.into_iter().enumerate() {
            let this_tree = self.ranks[rank].take();
            let mut trees = this_tree
                .into_iter()
                .chain(other_tree.into_iter())
                .chain(carry_tree.take().into_iter());

            if let Some(mut first_tree) = trees.next() {
                if let Some(second_tree) = trees.next() {
                    first_tree.merge(second_tree);
                    carry_tree = Some(first_tree);

                    self.ranks[rank] = trees.next();
                } else {
                    self.ranks[rank] = Some(first_tree);
                }
            }
        }

        let mut next_rank = other_rank;
        while let Some(carry) = carry_tree.take() {
            if let Some(mut this_tree) = self.ranks.get_mut(next_rank).and_then(|t| t.take()) {
                this_tree.merge(carry);
                carry_tree = Some(this_tree);
            } else if next_rank < self.ranks.len() {
                self.ranks[next_rank] = Some(carry);
            } else {
                self.ranks.push(Some(carry))
            }

            next_rank += 1;
        }
    }
}

impl<K: Ord, D> Heap<K, D> for BinomialHeap<K, D> {
    type EntryRef = ();

    fn insert(&mut self, entry: HeapEntry<K, D>) {
        let new_heap = BinomialHeap {
            ranks: vec![Some(BinomialTree {
                root: entry,
                childs: Vec::new(),
            })],
        };

        self.merge(new_heap);
    }

    fn delete_min(&mut self) -> Option<HeapEntry<K, D>> {
        if let Some(min_rank) = self
            .ranks
            .iter()
            .enumerate()
            .filter_map(|(idx, opt)| opt.as_ref().map(|v| (idx, v)))
            .min_by_key(|v| &v.1.root.key)
            .map(|v| v.0)
        {
            let BinomialTree { root, childs } = self.ranks[min_rank].take().unwrap();
            let tmp_heap = BinomialHeap {
                ranks: childs.into_iter().map(Some).collect::<Vec<_>>(),
            };

            self.merge(tmp_heap);

            Some(root)
        } else {
            None
        }
    }
}

struct RandomizedMeldableHeap<K, D> {
    root: Option<Box<Node<K, D>>>,
}

struct Node<K, D> {
    value: HeapEntry<K, D>,
    left: RandomizedMeldableHeap<K, D>,
    right: RandomizedMeldableHeap<K, D>,
}

impl<K: Ord, D> RandomizedMeldableHeap<K, D> {
    fn new() -> Self {
        RandomizedMeldableHeap::<K, D> { root: None }
    }

    fn meld(&mut self, mut other: Self) {
        if let Some(mut root) = self.root.take() {
            if let Some(mut other) = other.root.take() {
                if root.value.key > other.value.key {
                    mem::swap(&mut root, &mut other);
                }

                let other = RandomizedMeldableHeap { root: Some(other) };
                if rand::random() {
                    root.right.meld(other);
                } else {
                    root.left.meld(other);
                }
            }

            self.root = Some(root);
        } else {
            *self = other
        }
    }
}

impl<K: Ord, D> Heap<K, D> for RandomizedMeldableHeap<K, D> {
    type EntryRef = ();

    fn insert(&mut self, entry: HeapEntry<K, D>) {
        let node = Node {
            value: entry,
            left: RandomizedMeldableHeap { root: None },
            right: RandomizedMeldableHeap { root: None },
        };

        let other = RandomizedMeldableHeap {
            root: Some(Box::new(node)),
        };

        self.meld(other)
    }

    fn delete_min(&mut self) -> Option<HeapEntry<K, D>> {
        if let Some(root) = self.root.take() {
            let Node {
                value,
                mut left,
                right,
            } = *root;

            left.meld(right);
            *self = left;

            Some(value)
        } else {
            None
        }
    }
}

fn main() {
    println!("heap,operation,n,time");
    for j in 0..15 {
        test(1000000, 131254153214 + j);
    }
}

fn test(num_vals: usize, seed: u64) {
    let mut vals = (0..num_vals).into_iter().collect::<Vec<_>>();

    let mut rng = StdRng::seed_from_u64(seed);
    vals.shuffle(&mut rng);

    let mut binary_heap = BinaryHeap::new();
    let mut binomial_heap = BinomialHeap::new();
    let mut randomized_heap = RandomizedMeldableHeap::new();

    for i in 0..vals.len() {
        let start = Instant::now();
        binary_heap.insert(HeapEntry {
            key: vals[i],
            data: (),
        });
        let binary_insert_time = start.elapsed().as_nanos();

        let start = Instant::now();
        binomial_heap.insert(HeapEntry {
            key: vals[i],
            data: (),
        });
        let binomial_insert_time = start.elapsed().as_nanos();

        let start = Instant::now();
        randomized_heap.insert(HeapEntry {
            key: vals[i],
            data: (),
        });
        let randomized_insert_time = start.elapsed().as_nanos();

        println!("binary,insert,{},{binary_insert_time}", i / 10000);
        println!("binomial,insert,{},{binomial_insert_time}", i / 10000);
        println!("randomized,insert,{},{randomized_insert_time}", i / 10000);
    }

    for i in 0..vals.len() {
        let start = Instant::now();
        let val1 = binary_heap.delete_min().unwrap().key;
        let binary_delete_time = start.elapsed().as_nanos();

        let start = Instant::now();
        let val2 = binomial_heap.delete_min().unwrap().key;
        let binomial_delete_time = start.elapsed().as_nanos();

        let start = Instant::now();
        let val3 = randomized_heap.delete_min().unwrap().key;
        let randomized_delete_time = start.elapsed().as_nanos();

        assert_eq!(val1, val2);
        assert_eq!(val1, val3);

        println!("binary,delete,{},{binary_delete_time}", i / 10000);
        println!("binomial,delete,{},{binomial_delete_time}", i / 10000);
        println!("randomized,delete,{},{randomized_delete_time}", i / 10000);
    }
}
