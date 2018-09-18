package com.htmlism.lexorank
package storage

trait SqlStorage[F[_], K, R] extends Storage[F, K, R]