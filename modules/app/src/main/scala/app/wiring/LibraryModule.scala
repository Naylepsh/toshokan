package app.wiring

import cats.effect.IO
import doobie.Transactor
import http.View.NavBarItem
import library.asset.*
import library.author.{AuthorController, AuthorRepository, AuthorView}
import library.category.{CategoryRepository, CategoryService}

case class LibraryModule[F[_]](
    assetRepository: AssetRepository[F],
    assetService: AssetService[F],
    assetController: AssetController[F],
    categoryRepository: CategoryRepository[F],
    categoryService: CategoryService[F],
    authorRepository: AuthorRepository[F],
    authorController: AuthorController[F]
)

object LibraryModule:
  def make(
      xa: Transactor[IO],
      navBarItems: List[NavBarItem]
  ): LibraryModule[IO] =
    val assetRepository    = AssetRepository.make[IO](xa)
    val categoryRepository = CategoryRepository.make[IO](xa)
    val authorRepository   = AuthorRepository.make[IO](xa)
    val assetService       = AssetService.make(assetRepository)
    val categoryService    = CategoryService.make[IO](categoryRepository)
    val assetView          = AssetView(navBarItems)
    val assetController =
      AssetController(assetService, categoryService, assetView)
    val authorView       = AuthorView(navBarItems)
    val authorController = AuthorController(authorRepository, assetService, authorView)

    LibraryModule(
      assetRepository = assetRepository,
      assetService = assetService,
      assetController = assetController,
      categoryRepository = categoryRepository,
      categoryService = categoryService,
      authorRepository = authorRepository,
      authorController = authorController
    )
