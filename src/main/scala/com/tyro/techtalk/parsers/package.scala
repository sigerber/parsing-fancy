package com.tyro.techtalk

package object parsers {

  implicit class IterableWithMapByWrapper[T](val iterable: Iterable[T]) extends AnyVal {
    def mapBy[K](extractKey: T => K): Map[K, T] = {
      iterable.map { x => extractKey(x) -> x }.toMap
    }
  }
}
