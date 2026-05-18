package app.wiring

import cats.effect.IO
import doobie.Transactor
import http.View.NavBarItem
import library.asset.*
import library.author.*
import library.category.{CategoryRepository, CategoryService}

case class LibraryModule(
    assetRepository: AssetRepository,
    assetService: AssetService,
    assetController: AssetController,
    categoryRepository: CategoryRepository,
    categoryService: CategoryService,
    authorRepository: AuthorRepository,
    authorService: AuthorService,
    authorView: AuthorView,
    authorController: AuthorController
)

object LibraryModule:
  def make(
      xa: Transactor[IO],
      navBarItems: List[NavBarItem]
  ): LibraryModule =
    val assetRepository    = AssetRepository.make
    val categoryRepository = CategoryRepository.make
    val authorRepository   = AuthorRepository.make
    val assetService       = AssetService.make(assetRepository, xa)
    val categoryService    = CategoryService.make(categoryRepository, xa)
    val authorService      = AuthorService.make(authorRepository, xa)
    val assetView          = AssetView(navBarItems)
    val assetController =
      AssetController(assetService, categoryService, assetView)
    val authorView = AuthorView(navBarItems)
    val authorController =
      AuthorController(authorService, assetService, authorView)

    LibraryModule(
      assetRepository = assetRepository,
      assetService = assetService,
      assetController = assetController,
      categoryRepository = categoryRepository,
      categoryService = categoryService,
      authorRepository = authorRepository,
      authorService = authorService,
      authorView = authorView,
      authorController = authorController
    )
