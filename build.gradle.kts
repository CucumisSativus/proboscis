plugins {
  kotlin("jvm") version "1.8.0"
  id("org.jlleitschuh.gradle.ktlint") version "11.3.1"
  application
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
  mavenCentral()
}

dependencies {
  testImplementation(kotlin("test"))
  implementation("io.arrow-kt:arrow-core:1.2.0-RC")
}

tasks.test {
  useJUnitPlatform()
}

kotlin {
  jvmToolchain(18)
}

application {
  mainClass.set("MainKt")
}
