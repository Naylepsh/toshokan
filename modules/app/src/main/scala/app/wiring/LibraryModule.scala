package app.wiring

import cats.effect.IO
import doobie.Transactor
import http.View.NavBarItem
import library.asset.*
import library.author.{AuthorController, AuthorRepository, AuthorView}
import library.category.{CategoryRepository, CategoryService}

case class LibraryModule[F[_]](
    assetRepository: AssetRepository,
    assetService: AssetService[F],
    assetController: AssetController[F],
    categoryRepository: CategoryRepository,
    categoryService: CategoryService[F],
    authorRepository: AuthorRepository,
    authorView: AuthorView,
    authorController: AuthorController[F]
)

object LibraryModule:
  def make(
      xa: Transactor[IO],
      navBarItems: List[NavBarItem]
  ): LibraryModule[IO] =
    val assetRepository    = AssetRepository.make
    val categoryRepository = CategoryRepository.make
    val authorRepository   = AuthorRepository.make
    val assetService       = AssetService.make(assetRepository, xa)
    val categoryService    = CategoryService.make[IO](categoryRepository, xa)
    val assetView          = AssetView(navBarItems)
    val assetController =
      AssetController(assetService, categoryService, assetView)
    val authorView = AuthorView(navBarItems)
    val authorController =
      AuthorController(authorRepository, assetService, authorView, xa)

    LibraryModule(
      assetRepository = assetRepository,
      assetService = assetService,
      assetController = assetController,
      categoryRepository = categoryRepository,
      categoryService = categoryService,
      authorRepository = authorRepository,
      authorView = authorView,
      authorController = authorController
    )
